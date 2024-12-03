package js7.data.value

import cats.effect.IO
import cats.instances.vector.*
import cats.syntax.traverse.*
import io.circe.syntax.*
import io.circe.{Decoder, DecodingFailure, Encoder, Json, JsonNumber, JsonObject}
import java.util.Objects.requireNonNull
import javax.annotation.{Nonnull, Nullable}
import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Checked.{catchExpected, catchNonFatalFlatten}
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.value.ValuePrinter.quoteString
import js7.data.value.ValueType.UnexpectedValueTypeProblem
import js7.data.value.expression.ExprFunction
import scala.collection.View
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions
import scala.math.BigDecimal.RoundingMode
import scala.util.control.NonFatal

sealed trait Value:
  def valueType: ValueType

  def release: IO[Unit] = IO.unit

  def asString: Checked[String] =
    as[StringValue].map(_.string)

  final def toStringValueString: Checked[String] =
    toStringValue.map(_.string)

  /** Soft conversion to StringValue. */
  def toStringValue: Checked[StringValue] =
    as[StringValue]

  def missingToNone: Option[Value] =
    Some(this)

  def missingToEmptyString: Value = this

  final def maybe: Option[Value] =
    (this != MissingValue) ? this

  def as[V <: Value](using V: Value.Companion[V]): Checked[V] =
    if valueType is V then
      Right(this.asInstanceOf[V])
    else
      Left(UnexpectedValueTypeProblem(V, this))

  def asMissingOr[V <: Value](using V: Value.Companion[V]): Checked[V | MissingValue] =
    this match
      case MissingValue => Right(MissingValue)
      case _ => as[V]

  /** Similar to as[V], returns MissingValue as None. */
  def asMaybe[V <: Value](using V: Value.Companion[V]): Checked[Option[V]] =
    this match
      case MissingValue => Right(None)
      case _ => as[V].map(Some(_))

  final def asMaybeNumber: Checked[Option[BigDecimal]] =
    asNumber.map(Some(_))

  final def asInt: Checked[Int] =
    asNumber.flatMap(o => catchExpected[ArithmeticException](
      o.toIntExact))

  final def asLong: Checked[Long] =
    asNumber.flatMap(o => catchExpected[ArithmeticException]
      (o.toLongExact))

  final def asLongIgnoreFraction: Checked[Long] =
    asNumber
      .flatMap(o =>
        catchExpected[Exception](o
          .setScale(0, RoundingMode.DOWN)
          .toLongExact))

  final def asNumber: Checked[BigDecimal] =
    as[NumberValue].map(_.number)

  final def asDuration: Checked[FiniteDuration] =
    asNumber.map(_.setScale(3, RoundingMode.DOWN).s)

  /** Soft conversion to NumberValue. */
  def toNumberValue: Checked[NumberValue] =
    as[NumberValue]

  def asBoolean: Checked[Boolean] =
    as[BooleanValue].map(_.booleanValue)

  /** Soft conversion to BooleanValue. */
  def toBooleanValue: Checked[BooleanValue] =
    as[BooleanValue]

  def asList: Checked[Seq[Value]] =
    as[ListValue].map(_.elements)

  final def asObject: Checked[Map[String, Value]] =
    as[ObjectValue].map(_.nameToValue)

  @javaApi @Nullable
  def toJava: java.lang.Object | Null/*for MissingValue*/

  def convertToString: String


object Value:
  /** Subset of Java type that are usable for `ofAny`.
    * No recursive type possible here.
    */
  type SimpleJava = Value |
    String |
    Boolean | java.lang.Boolean |
    Int | java.lang.Integer |
    Long | java.lang.Long |
    BigDecimal | java.math.BigDecimal |
    Double | java.lang.Double |
    Throwable

  @javaApi
  def of(value: String): StringValue =
    StringValue(value)

  @javaApi
  def of(value: java.math.BigDecimal): NumberValue =
    NumberValue(value)

  @javaApi
  def of(value: Long): NumberValue =
    NumberValue(BigDecimal(value))

  @javaApi
  def of(value: Int): NumberValue =
    NumberValue(BigDecimal(value))

  @javaApi
  def of(value: Boolean): BooleanValue =
    BooleanValue(value)

  def ofSimpleJava(value: SimpleJava): Checked[Value] =
    ofAny(value)

  def ofAny(value: Any): Checked[Value] =
    catchNonFatalFlatten:
      value match
        case v: Value => Right(v)
        case v: String => Right(StringValue(v))
        case v: Boolean => Right(BooleanValue(v))
        case v: Int => Right(NumberValue(v))
        case v: Long => Right(NumberValue(v))
        case v: java.lang.Boolean => Right(BooleanValue(v))
        case v: java.lang.Integer => Right(NumberValue(BigDecimal(v)))
        case v: java.lang.Long => Right(NumberValue(BigDecimal(v)))
        case v: BigDecimal => Right(NumberValue(v))
        case v: java.math.BigDecimal => Right(NumberValue(v))
        case v: Double =>
          if v.isNaN then
            Right(MissingValue)
          else v match
            case Double.PositiveInfinity => Left(Problem("Double.PositiveInfinity is an invalid value"))
            case Double.NegativeInfinity => Left(Problem("Double.NegativeInfinity is an invalid value"))
            case v: Double => Right(NumberValue(BigDecimal(v)))
        case v: java.lang.Double => Right(NumberValue(BigDecimal(v)))
        case t: Throwable => Left(Problem(t.toStringWithCauses)) // LiveBeanMapView may return Throwable
        case _ => Left(Problem(s"Unknown type for a Value: ${value.getClass.getName}"))

  implicit val jsonEncoder: Encoder[Value] =
    case StringValue(o) => Json.fromString(o)
    case NumberValue(n) => n match
      case NumberValue.Zero.number => NumberValue.ZeroJson
      case NumberValue.One.number => NumberValue.OneJson
      case o => Json.fromBigDecimal(o)
    case BooleanValue(o) => Json.fromBoolean(o)
    case ListValue(values) => Json.fromValues(values map jsonEncoder.apply)
    case ObjectValue(values) => Json.fromJsonObject(JsonObject.fromIterable(values.view.mapValues(jsonEncoder.apply)))
    case MissingValue => Json.Null
    case v: FunctionValue => sys.error(s"${v.valueType.name} cannot be JSON encoded: $v")

  implicit val jsonDecoder: Decoder[Value] =
    val Zero = Right(NumberValue.Zero)
    val One = Right(NumberValue.One)
    val False = Right(BooleanValue.False)
    val True = Right(BooleanValue.True)
    c => {
      val j = c.value
      if j.isString then
        Right(StringValue(j.asString.get))
      else if j.isNumber then
        j.asNumber.get match
          case NumberValue.ZeroJsonNumber => Zero
          case NumberValue.OneJsonNumber => One
          case number => number.toBigDecimal match
            case Some(o) => Right(NumberValue(o))
            case None => Left(DecodingFailure(s"JSON number is not representable as a Java BigDecimal: $j", c.history))
      else if j.isBoolean then
        if j.asBoolean.get then True else False
      else if j.isArray then
        j.asArray.get.traverse(jsonDecoder.decodeJson) map ListValue.apply
      else if j.isObject then
        j.asObject.get.toVector
          .traverse { case (k, v) => jsonDecoder.decodeJson(v).map(k -> _) }
          .map(o => ObjectValue(o.toMap))
      else if j.isNull then
        Right(MissingValue)
      else
        Left(DecodingFailure(s"Unknown value JSON type: ${j.getClass.shortClassName}", c.history))
    }

  trait Companion[V <: Value] extends ValueType:
    implicit val implicitCompanion: Companion[V] = this

  object convenience:
    implicit def convenientBooleanValue(b: Boolean): BooleanValue =
      BooleanValue(b)

    implicit def convenientStringValue(string: String): StringValue =
      StringValue(string)

    implicit def convenientNumberValue(number: Int): NumberValue =
      NumberValue(number)

    implicit def convenientNumberValue(number: Long): NumberValue =
      NumberValue(number)

    implicit def convenientNumberValue(number: BigDecimal): NumberValue =
      NumberValue(number)

    implicit def convenientListValue(seq: Seq[Value]): ListValue =
      ListValue(seq)

/** ValueType for any Value. */
object AnyValue extends ValueType:
  val name = "Any"

  override infix def is(t: ValueType): Boolean =
    true


/** Any value except the MissingValue is a GoodValue.
  */
sealed trait GoodValue extends Value
object GoodValue:
  trait Companion[V <: GoodValue] extends Value.Companion[V]:
    override infix def is(t: ValueType): Boolean =
      (companion eq t) || super.is(t)

  implicit val companion: Companion[GoodValue] =
    new Companion[GoodValue]:
      val name = "GoodValue"


/** StringValue
  */
final case class StringValue(string: String) extends GoodValue:
  requireNonNull(string)

  def valueType: ValueType = StringValue

  override def toNumberValue: Checked[NumberValue] =
    try Right(NumberValue(BigDecimal(string)))
    catch { case NonFatal(_: NumberFormatException) =>
      Left(Problem.pure("Not a valid number: " + string.truncateWithEllipsis(50)))
    }

  override def toBooleanValue: Checked[BooleanValue] = string match
    case "true" => Right(BooleanValue.True)
    case "false" => Right(BooleanValue.False)
    case _ => super.toBooleanValue

  @javaApi @Nonnull def toJava: String =
    string

  def convertToString: String = string

  override def toString: String =
    ValuePrinter.quoteString(string.truncateWithEllipsis(200, showLength = true))

object StringValue extends GoodValue.Companion[StringValue], ValueType.Simple:
  val name = "String"
  val empty: StringValue = StringValue("")

  @javaApi def of(value: String): StringValue =
    StringValue(value)

  given Ordering[StringValue] = Ordering.by(_.string)


/** NumberValue for any numeric value.
  */
final case class NumberValue(number: BigDecimal) extends GoodValue:
  def valueType: ValueType = NumberValue

  override def toStringValue: Right[Problem, StringValue] =
    Right(StringValue(number.toString))

  override def toBooleanValue: Checked[BooleanValue] =
    if number == NumberValue.One.number then Right(BooleanValue.True)
    else
    if number == NumberValue.Zero.number then Right(BooleanValue.False)
    else
      super.toBooleanValue

  @javaApi @Nonnull
  def toJava: java.math.BigDecimal =
    number.bigDecimal

  @javaApi @Nonnull
  def toBigDecimal: java.math.BigDecimal =
    number.bigDecimal

  def convertToString: String =
    number.toString

  override def toString = convertToString

object NumberValue extends GoodValue.Companion[NumberValue], ValueType.Simple:
  val name = "Number"
  val Zero: NumberValue = NumberValue(0)
  val One: NumberValue = NumberValue(1)
  private[value] val ZeroJson: Json = 0.asJson
  private[value] val OneJson: Json = 1.asJson
  private[value] val ZeroJsonNumber: JsonNumber = ZeroJson.asNumber.get
  private[value] val OneJsonNumber: JsonNumber = OneJson.asNumber.get

  def fromString(number: String): Checked[NumberValue] =
    try
      Right(NumberValue(BigDecimal(number)))
    catch { case e: NumberFormatException => Problem(Option(e.getMessage) getOrElse e.toString)}

  @javaApi
  def of(value: java.math.BigDecimal): NumberValue =
    NumberValue(value)

  @javaApi
  def of(value: Long): NumberValue =
    NumberValue(BigDecimal(value))

  @javaApi
  def of(value: Integer): NumberValue =
    NumberValue(BigDecimal(value))

  given Ordering[NumberValue] = Ordering.by(_.number)
  given Encoder[NumberValue] = summon[Encoder[BigDecimal]].contramap(_.number)
  given Decoder[NumberValue] = summon[Decoder[BigDecimal]].map(NumberValue(_))


/** BooleanValue.
  */
final case class BooleanValue(booleanValue: Boolean) extends GoodValue:
  def valueType: ValueType = BooleanValue

  override def toNumberValue: Checked[NumberValue] =
    Right(if booleanValue then NumberValue.One else NumberValue.Zero)

  @javaApi @Nonnull def toJava: java.lang.Boolean =
    java.lang.Boolean.valueOf(booleanValue)

  override def toStringValue: Right[Problem, StringValue] =
    Right(StringValue(convertToString))

  def convertToString: String =
    booleanValue.toString

  override def toString =
    convertToString

object BooleanValue extends GoodValue.Companion[BooleanValue], ValueType.Simple:
  val name = "Boolean"
  val True: BooleanValue = BooleanValue(true)
  val False: BooleanValue = BooleanValue(false)

  @javaApi def of(value: Boolean): BooleanValue = BooleanValue(value)


/** A list of values of undeclared type.
  */
final case class ListValue private(elements: Vector[Value]) extends GoodValue:
  def valueType: ValueType = ListValue

  @javaApi @Nonnull def toJava: java.util.List[Value] =
    elements.asJava

  def convertToString: String = elements.mkString("[", ", ", "]")

  override def toString: String = convertToString

object ListValue extends GoodValue.Companion[ListValue], ValueType.Compound:
  val name = "List"
  val empty: ListValue = ListValue(Vector.empty)

  def apply(elements: Iterable[Value]): ListValue =
    new ListValue(elements.toVector)

  @javaApi @Nonnull def of(@Nonnull values: java.util.List[Value]): ListValue =
    ListValue(values.asScala.toVector)

  @javaApi @Nonnull def of(@Nonnull values: Array[Value]): ListValue =
    ListValue(values.toVector)

final case class ListType(elementType: ValueType)
extends ValueType.Compound:
  def name = "List"


/** An object with fields of undeclared type.
  */
final case class ObjectValue(nameToValue: Map[String, Value]) extends GoodValue:
  def valueType: ValueType = ObjectValue

  @javaApi @Nonnull def toJava: java.util.Map[String, Value] =
    nameToValue.asJava

  def convertToString: String = nameToValue
    .map { case (k, v) => quoteString(k) + ":" + v }
    .mkString("{", ", ", "}")

  override def toString: String = convertToString

object ObjectValue extends GoodValue.Companion[ObjectValue], ValueType.Compound:
  val name = "Object"
  val empty: ObjectValue = ObjectValue(Map.empty)

  @javaApi @Nonnull
  def of(@Nonnull nameToValue: java.util.Map[String, Value]): ObjectValue =
    ObjectValue(nameToValue.asScala.toMap)

  @throws
  def unsafeSimpleJava(nameToValue: (String, Value.SimpleJava)*): ObjectValue =
    apply:
      nameToValue.view.map: (k, v) =>
        k -> Value.ofSimpleJava(v).orThrow
      .toMap


final case class ObjectType(nameToType: Map[String, ValueType])
extends ValueType.Compound:
  def name = "Object"

final case class FunctionValue(function: ExprFunction) extends GoodValue:
  def valueType: ValueType = FunctionValue

  def toJava: Object =
    throw new RuntimeException("FunctionValue cannot be converted to a Java value")

  def convertToString: String =
    function.toString

  override def toString = function.toString
object FunctionValue extends GoodValue.Companion[FunctionValue], ValueType:
  val name = "Function"


type MissingValue = MissingValue.type

/** The inapplicable value.
 *
 * Similar to Scala None (but there is no Some).
 * Unlike SQL null, this MissingValue equals itself. */
case object MissingValue extends Value, ValueType.Simple:
  val valueType: ValueType = MissingValue

  val name = "Missing"

  override def missingToNone: None.type =
    None

  override def missingToEmptyString: Value =
    StringValue.empty

  @javaApi @Nullable def toJava: Null =
    null

  override val convertToString = ""

  override val toString = "missing"


sealed trait ValueType:
  def name: String

  infix def is(t: ValueType): Boolean =
    this eq t

  override def toString: String = name

object ValueType:
  sealed trait Simple extends ValueType
  sealed trait Compound extends ValueType

  private val nameToSimpleType = View(AnyValue, StringValue, BooleanValue, NumberValue)
    .toKeyedMap(_.name)

  private val listTypeField = "TYPE" -> Json.fromString("List")
  private val objectTypeField = "TYPE" -> Json.fromString("Object")

  implicit val jsonEncoder: Encoder[ValueType] =
    case ListType(elementType) =>
      Json.obj(
        listTypeField,
        "elementType" -> jsonEncoder(elementType))

    case ObjectType(fields) =>
      Json.fromFields(
        new View.Single(objectTypeField) ++
          fields.view.map { case (k, v) => k -> jsonEncoder(v) })

    case AnyValue =>
      Json.Null

    case o: ValueType =>
      Json.fromString(o.name)

  implicit val jsonDecoder: Decoder[ValueType] =
    c => {
      val json = c.value
      if json.isString then
        nameToSimpleType.checked(c.value.asString.get)
          .toDecoderResult(c.history)
      else if json.isObject then
        for
          typ <- c.get[String]("TYPE")
          valueType <- typ match
            case "List" =>
              c.get[ValueType]("elementType")(jsonDecoder)
                .map(ListType(_))

            case "Object" =>
              c.value.asObject.get
                .toIterable
                .view
                .filter(_._1 != "TYPE")
                .toVector
                .traverse { case (k, v) => jsonDecoder
                  .decodeJson(v)
                  .map(k -> _)
                }
                .map(fields => ObjectType(fields.toMap))

            case typeName =>
              Left(DecodingFailure(s"Unknown ValueType: $typeName", c.history))
        yield valueType
      else
        Left(DecodingFailure("ValueType expected", c.history))
    }

  final case class UnknownNameInExpressionProblem(name: String) extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "name" -> name)
  object UnknownNameInExpressionProblem:
    val default: UnknownNameInExpressionProblem = new UnknownNameInExpressionProblem("missing")

    def apply(name: String): UnknownNameInExpressionProblem =
      if name == default.name then
        default
      else
        new UnknownNameInExpressionProblem(name)

  final case class ErrorInExpressionProblem(errorMessage: String) extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "errorMessage" -> errorMessage)

  final case class UnexpectedValueTypeProblem(expectedType: ValueType, value: Value)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "expectedType" -> expectedType.name,
      "value" -> (value.valueType.name + ": " + value.toString.truncateWithEllipsis(30)))
