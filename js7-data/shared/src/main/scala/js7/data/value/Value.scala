package js7.data.value

import cats.instances.vector._
import cats.syntax.traverse._
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, Json, JsonObject}
import java.util.Objects.requireNonNull
import javax.annotation.{Nonnull, Nullable}
import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax._
import js7.data.value.ValuePrinter.quoteString
import js7.data.value.ValueType.{MissingValueProblem, UnexpectedValueTypeProblem}
import scala.collection.View
import scala.jdk.CollectionConverters._
import scala.language.implicitConversions
import scala.util.control.NonFatal

sealed trait Value
{
  def valueType: ValueType

  final def toStringValueString: Checked[String] =
    toStringValue.map(_.string)

  def asString: Checked[String] =
    asStringValue.map(_.string)

  def toStringValue: Checked[StringValue] =
    asStringValue

  def asStringValue: Checked[StringValue] =
    Left(UnexpectedValueTypeProblem(StringValue, this))

  final def asLong: Checked[Long] =
    asNumber.map(_.toLong)

  final def asNumber: Checked[BigDecimal] =
    asNumberValue.map(_.number)

  def toNumberValue: Checked[NumberValue] =
    asNumberValue

  def asNumberValue: Checked[NumberValue] =
    Left(UnexpectedValueTypeProblem(NumberValue, this))

  def asBoolean: Checked[Boolean] =
    asBooleanValue.map(_.booleanValue)

  def toBooleanValue: Checked[BooleanValue] =
    asBooleanValue

  def asBooleanValue: Checked[BooleanValue] =
    Left(UnexpectedValueTypeProblem(BooleanValue, this))

  def asList: Checked[Seq[Value]] =
    asListValue.map(_.elements)

  def asListValue: Checked[ListValue] =
    Left(UnexpectedValueTypeProblem(ListValue, this))

  final def asObject: Checked[Map[String, Value]] =
    asObjectValue.map(_.nameToValue)

  def asObjectValue: Checked[ObjectValue] =
    Left(UnexpectedValueTypeProblem(ObjectValue, this))

  @javaApi @Nullable/*for NullValue ???*/
  def toJava: java.lang.Object

  def convertToString: String
}

object Value
{
  //def apply(value: String) = StringValue(value)
  //def apply(value: BigDecimal) = NumberValue(value)
  //def apply(value: Long) = NumberValue(BigDecimal(value))
  //def apply(value: Int) = NumberValue(BigDecimal(value))
  //def apply(value: Boolean) = BooleanValue(value)

  @javaApi def of(value: String) = StringValue(value)
  @javaApi def of(value: java.math.BigDecimal) = NumberValue(value)
  @javaApi def of(value: Long) = NumberValue(BigDecimal(value))
  @javaApi def of(value: Int) = NumberValue(BigDecimal(value))
  @javaApi def of(value: Boolean) = BooleanValue(value)

  def catchNonFatal[V <: Value](body: => V): Value =
    try body
    catch { case NonFatal(t) => ErrorValue(Problem.fromThrowable(t)) }

  implicit val jsonEncoder: Encoder[Value] = {
    case StringValue(o) => Json.fromString(o)
    case NumberValue.Zero => NumberValue.ZeroJson
    case NumberValue.One => NumberValue.OneJson
    case NumberValue(o) => Json.fromBigDecimal(o)
    case BooleanValue(o) => Json.fromBoolean(o)
    case ListValue(values) => Json.fromValues(values map jsonEncoder.apply)
    case ObjectValue(values) => Json.fromJsonObject(JsonObject.fromIterable(values.view.mapValues(jsonEncoder.apply)))
    case NullValue => Json.Null
    case v: IsErrorValue => sys.error(s"IsErrorValue cannot be JSON encoded: $v")
  }

  implicit val jsonDecoder: Decoder[Value] = {
    val Zero = Right(NumberValue.Zero)
    val One = Right(NumberValue.One)
    val False = Right(BooleanValue.False)
    val True = Right(BooleanValue.True)
    c => {
      val j = c.value
      if (j.isString)
        Right(StringValue(j.asString.get))
      else if (j.isNumber)
        j.asNumber.get match {
          case NumberValue.ZeroJson => Zero
          case NumberValue.OneJson => One
          case number => number.toBigDecimal match {
            case Some(o) => Right(NumberValue(o))
            case None => Left(DecodingFailure(s"JSON number is not representable as a Java BigDecimal: $j", c.history))
          }
        }
      else if (j.isBoolean)
        if (j.asBoolean.get) True else False
      else if (j.isArray)
        j.asArray.get.traverse(jsonDecoder.decodeJson) map ListValue.apply
      else if (j.isObject)
        j.asObject.get.toVector
          .traverse { case (k, v) => jsonDecoder.decodeJson(v).map(k -> _) }
          .map(o => ObjectValue(o.toMap))
      else if (j.isNull)
        Right(NullValue)
      else
        Left(DecodingFailure(s"Unknown value JSON type: ${j.getClass.simpleScalaName}", c.history))
    }
  }

  object convenience {
    implicit def convenientStringValue(string: String): Value = StringValue(string)
    implicit def convenientNumericValue(number: Int): Value = NumberValue(number)
  }
}

final case class StringValue(string: String) extends Value
{
  requireNonNull(string)

  def valueType = StringValue

  override def asStringValue = Right(this)

  override def toNumberValue =
    try Right(NumberValue(BigDecimal(string)))
    catch { case NonFatal(_: NumberFormatException) =>
      Left(Problem.pure(s"Not a valid number: " + string.truncateWithEllipsis(50)))
    }

  override def toBooleanValue = string match {
    case "true" => Right(BooleanValue.True)
    case "false" => Right(BooleanValue.False)
    case _ => super.toBooleanValue
  }

  def toJava = string
  def convertToString = string

  override def toString = ValuePrinter.quoteString(string)
}

object StringValue extends ValueType.Simple
{
  val name = "String"

  @javaApi def of(value: String) = StringValue(value)
}

final case class NumberValue(number: BigDecimal) extends Value
{
  def valueType = NumberValue

  override def toStringValue = Right(StringValue(number.toString))

  override def asNumberValue = Right(this)

  override def toBooleanValue =
    if (number == NumberValue.One.number) Right(BooleanValue.True)
    else
    if (number == NumberValue.Zero.number) Right(BooleanValue.False)
    else
      super.toBooleanValue

  @javaApi @Nonnull
  def toJava: java.math.BigDecimal =
    number.bigDecimal

  @javaApi @Nonnull
  def toBigDecimal: java.math.BigDecimal =
    number.bigDecimal

  def convertToString = number.toString

  override def toString = convertToString
}

object NumberValue extends ValueType.Simple
{
  val name = "Number"
  val Zero = NumberValue(0)
  val One = NumberValue(1)
  val ZeroJson = 0.asJson
  val OneJson = 1.asJson

  def fromString(number: String): Checked[NumberValue] =
    try
      Right(NumberValue(BigDecimal(number)))
    catch { case e: NumberFormatException => Problem(Option(e.getMessage) getOrElse e.toString)}

  @javaApi def of(value: java.math.BigDecimal) = NumberValue(value)
  @javaApi def of(value: Long) = NumberValue(BigDecimal(value))
  @javaApi def of(value: Integer) = NumberValue(BigDecimal(value))
}

final case class BooleanValue(booleanValue: Boolean) extends Value
{
  def valueType = BooleanValue

  override def toNumberValue =
    Right(if (booleanValue) NumberValue.One else NumberValue.Zero)

  override def asBooleanValue = Right(this)

  def toJava = java.lang.Boolean.valueOf(booleanValue)

  override def toStringValue = Right(StringValue(convertToString))

  def convertToString = booleanValue.toString

  override def toString = convertToString
}

object BooleanValue extends ValueType.Simple
{
  val name = "Boolean"
  val True = BooleanValue(true)
  val False = BooleanValue(false)

  @javaApi def of(value: Boolean) = BooleanValue(value)
}

final case class ListValue(elements: Vector[Value]) extends Value
{
  def valueType = ListValue

  override def asListValue = Right(this)

  def toJava = elements.asJava

  def convertToString = elements.mkString("[", ", ", "]")

  override def toString = convertToString
}

/** A list of values of undeclared type. */
object ListValue extends ValueType.Compound
{
  val name = "List"
  val empty = ListValue(Vector.empty)

  def apply(list: Seq[Value]): ListValue =
    new ListValue(list.toVector)

  @javaApi def of(values: java.util.List[Value]) = ListValue(values.asScala.toVector)
  @javaApi def of(values: Array[Value]) = ListValue(values.toVector)
}

final case class ListType(elementType: ValueType)
extends ValueType.Compound
{
  def name = "List"
}

/** An object with fields of undeclared type. */
final case class ObjectValue(nameToValue: Map[String, Value]) extends Value
{
  def valueType = ObjectValue

  override def asObjectValue = Right(this)

  def toJava = ???

  def convertToString = nameToValue
    .map { case (k, v) => quoteString(k) + ":" + v }
    .mkString("{", ", ", "}")

  override def toString = convertToString
}

object ObjectValue extends ValueType.Compound
{
  val name = "Object"
  val empty = ObjectValue(Map.empty)

  @javaApi @Nonnull def of(@Nonnull nameToValue: java.util.Map[String, Value]) =
    ObjectValue(nameToValue.asScala.toMap)
}

final case class ObjectType(nameToType: Map[String, ValueType])
extends ValueType.Compound
{
  def name = "Object"
}

/** Just a reminder that MissingValue could be an instance of a future IsErrorValue.
  * For example, division by zero. */
sealed trait IsErrorValue extends Value {
  def problem: Problem
}

/** A missing value due to a problem. */
final case class ErrorValue(problem: Problem) extends IsErrorValue {
  def valueType = ErrorValue

  def toJava = problem // ???

  override def convertToString = s"[Problem: $problem]"

  override def toString = convertToString
}
object ErrorValue extends ValueType {
  val name = "Problem"
}

/** A missing value due to a problem. */
final case class MissingValue(problem: Problem = MissingValueProblem) extends IsErrorValue {
  def valueType = MissingValue

  def toJava = problem // ???

  override def convertToString = {
    if (problem == MissingValueProblem) "[MissingValue]"
    else s"[MissingValue: $problem]"
  }

  override def toString = convertToString
}
object MissingValue extends ValueType {
  val name = "Missing"
}

/** The inapplicable value. */
case object NullValue extends Value with ValueType.Simple {
  def valueType = NullValue

  val name = "Null"

  def toJava = null // ???

  override def convertToString = ""

  override def toString = convertToString
}

sealed trait ValueType
{
  def name: String

  override def toString = name
}

object ValueType
{
  sealed trait Simple extends ValueType
  sealed trait Compound extends ValueType

  private val nameToSimpleType = View(StringValue, BooleanValue, NumberValue)
    .toKeyedMap(_.name)

  implicit val jsonEncoder: Encoder[ValueType] = {
    case ListType(elementType) => Json.obj(
      "TYPE" -> Json.fromString("List"),
      "elementType" -> elementType.asJson(jsonEncoder))

    case ObjectType(fields) =>
      Json.fromFields(
        new View.Single("TYPE" -> Json.fromString("Object")) ++
          fields.view.map { case (k, v) => k -> jsonEncoder(v) })

    case o: ValueType =>
      Json.fromString(o.name)
  }

  implicit val jsonDecoder: Decoder[ValueType] =
    c => {
      val json = c.value
      if (json.isString)
        nameToSimpleType.checked(c.value.asString.get)
          .toDecoderResult(c.history)
      else if (json.isObject)
        for {
          typ <- c.get[String]("TYPE")
          valueType <- typ match {
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
          }
        } yield valueType
      else
        Left(DecodingFailure("ValueType expected", c.history))
    }

  final case object MissingValueProblem extends Problem.ArgumentlessCoded

  final case class UnexpectedValueTypeProblem(valueType: ValueType, value: Value) extends Problem.Coded {
    def arguments = Map(
      "type" -> valueType.name,
      "value" -> value.toString.truncateWithEllipsis(30))
  }
}
