package js7.data.value

import cats.instances.vector._
import cats.syntax.traverse._
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, Json, JsonObject}
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax._
import js7.data.value.ValuePrinter.quoteString
import scala.jdk.CollectionConverters._
import scala.language.implicitConversions
import scala.util.control.NonFatal

sealed trait Value
{
  def valueType: ValueType

  def toStringValue: Checked[StringValue]

  def toNumber: Checked[NumberValue] =
    Left(InvalidExpressionTypeProblem(NumberValue, this))

  def toBoolean: Checked[BooleanValue] =
    Left(InvalidExpressionTypeProblem(BooleanValue, this))

  def toList: Checked[ListValue] =
    Left(InvalidExpressionTypeProblem(ListValue, this))

  def toObject: Checked[ObjectValue] =
    Left(InvalidExpressionTypeProblem(ObjectValue, this))

  @javaApi
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

  implicit val jsonEncoder: Encoder[Value] = {
    case StringValue(o) => Json.fromString(o)
    case NumberValue.Zero => NumberValue.ZeroJson
    case NumberValue.One => NumberValue.OneJson
    case NumberValue(o) => Json.fromBigDecimal(o)
    case BooleanValue(o) => Json.fromBoolean(o)
    case ListValue(values) => Json.fromValues(values map jsonEncoder.apply)
    case ObjectValue(values) => Json.fromJsonObject(JsonObject.fromIterable(values.view.mapValues(jsonEncoder.apply)))
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
  def valueType = StringValue

  def toStringValue = Right(this)

  override def toNumber =
    try Right(NumberValue(BigDecimal(string)))
    catch { case NonFatal(t) => Left(Problem.pure(t.toStringWithCauses)) }

  override def toBoolean = string match {
    case "true" => Right(BooleanValue.True)
    case "false" => Right(BooleanValue.False)
    case _ => super.toBoolean
  }

  def toJava = string
  def convertToString = string

  override def toString = ValuePrinter.quoteString(string)
}

object StringValue extends ValueType
{
  val name = "String"

  @javaApi def of(value: String) = StringValue(value)
}

final case class NumberValue(number: BigDecimal) extends Value
{
  def valueType = NumberValue

  def toStringValue = Right(StringValue(number.toString))

  override def toNumber = Right(this)

  override def toBoolean =
    if (number == NumberValue.One.number) Right(BooleanValue.True)
    else
    if (number == NumberValue.Zero.number) Right(BooleanValue.False)
    else
      super.toBoolean

  @javaApi @Nonnull
  def toJava: java.math.BigDecimal =
    number.bigDecimal

  @javaApi @Nonnull
  def toBigDecimal: java.math.BigDecimal =
    number.bigDecimal

  def convertToString = number.toString

  override def toString = convertToString
}

object NumberValue extends ValueType
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

  override def toNumber =
    Right(if (booleanValue) NumberValue.One else NumberValue.Zero)

  override def toBoolean = Right(BooleanValue(booleanValue))

  def toJava = java.lang.Boolean.valueOf(booleanValue)

  def toStringValue = Right(StringValue(convertToString))

  def convertToString = booleanValue.toString

  override def toString = convertToString
}

object BooleanValue extends ValueType
{
  val name = "Boolean"
  val True = BooleanValue(true)
  val False = BooleanValue(false)

  @javaApi def of(value: Boolean) = BooleanValue(value)
}

final case class ListValue(list: Seq[Value]) extends Value
{
  def valueType = ListValue

  override def toList = Right(this)

  def toJava = list.asJava

  def toStringValue = Right(StringValue(convertToString))

  def convertToString = list.mkString("[", ", ", "]")

  override def toString = convertToString
}

object ListValue extends ValueType
{
  val name = "List"

  @javaApi def of(values: java.util.List[Value]) = ListValue(values.asScala.toVector)
  @javaApi def of(values: Array[Value]) = ListValue(values.toVector)
}

final case class ObjectValue(nameToValue: Map[String, Value]) extends Value
{
  def valueType = ObjectValue

  override def toObject = Right(this)

  def toStringValue = Right(StringValue(convertToString))

  def toJava = ???

  def convertToString = nameToValue
    .map { case (k, v) => quoteString(k) + ":" + v }
    .mkString("{", ", ", "}")

  override def toString = convertToString
}

object ObjectValue extends ValueType
{
  val name = "Object"
  val empty = ObjectValue(Map.empty)

  @javaApi def of(values: java.util.List[Value]) = ListValue(values.asScala.toVector)
  @javaApi def of(values: Array[Value]) = ListValue(values.toVector)
}

sealed trait ValueType
{
  def name: String

  override def toString = name
}

object ValueType
{
  val values = Seq(StringValue, BooleanValue, NumberValue, ListValue, ObjectValue)
  private val nameToType = values.toKeyedMap(_.name)

  implicit val jsonEncoder: Encoder[ValueType] =
    o => Json.fromString(o.name)

  implicit val jsonDecoder: Decoder[ValueType] =
    cursor => (cursor.value.asString match {
      case None => Left(Problem("ValueType expected"))
      case Some(string) => nameToType.checked(string)
    }).toDecoderResult(cursor.history)
}

private case class InvalidExpressionTypeProblem(valueType: ValueType, value: Value) extends Problem.Coded {
  def arguments = Map(
    "type" -> valueType.name,
    "value" -> value.toString.truncateWithEllipsis(30))
}
