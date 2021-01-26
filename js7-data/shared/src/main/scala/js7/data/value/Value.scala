package js7.data.value

import cats.instances.vector._
import cats.syntax.traverse._
import io.circe.{Decoder, DecodingFailure, Encoder, Json, JsonObject}
import js7.base.annotation.javaApi
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.value.ValuePrinter.quoteString
import scala.jdk.CollectionConverters._
import scala.language.implicitConversions
import scala.util.control.NonFatal

sealed trait Value
{
  def toStringValue: Checked[StringValue]

  def toNumber: Checked[NumberValue] =
    Left(InvalidExpressionTypeProblem("Numeric", this))

  def toBoolean: Checked[BooleanValue] =
    Left(InvalidExpressionTypeProblem("Boolean", this))

  def toList: Checked[ListValue] =
    Left(InvalidExpressionTypeProblem("List", this))

  def toObject: Checked[ObjectValue] =
    Left(InvalidExpressionTypeProblem("Object", this))

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
  @javaApi def of(value: BigDecimal) = NumberValue(value)
  @javaApi def of(value: java.lang.Long) = NumberValue(BigDecimal(value))
  @javaApi def of(value: java.lang.Integer) = NumberValue(BigDecimal(value))
  @javaApi def of(value: java.lang.Boolean) = BooleanValue(value)

  implicit val jsonEncoder: Encoder[Value] = {
    case StringValue(o) => Json.fromString(o)
    case NumberValue(o) => Json.fromBigDecimal(o)
    case BooleanValue(o) => Json.fromBoolean(o)
    case ListValue(values) => Json.fromValues(values map jsonEncoder.apply)
    case ObjectValue(values) => Json.fromJsonObject(JsonObject.fromIterable(values.view.mapValues(jsonEncoder.apply)))
  }

  implicit val jsonDecoder: Decoder[Value] = c => {
    val j = c.value
    if (j.isString)
      Right(StringValue(j.asString.get))
    else if (j.isNumber)
      j.asNumber.get.toBigDecimal match {
        case Some(o) => Right(NumberValue(o))
        case None => Left(DecodingFailure(s"JSON number is not representable as a Java BigDecimal: $j", c.history))
      }
    else if (j.isBoolean)
      Right(BooleanValue(j.asBoolean.get))
    else if (j.isArray)
      j.asArray.get.traverse(jsonDecoder.decodeJson).map(ListValue.apply)
    else if (j.isObject)
      j.asObject.get.toVector
        .traverse { case (k, v) => jsonDecoder.decodeJson(v).map(k -> _) }
        .map(o => ObjectValue(o.toMap))
    else
      Left(DecodingFailure(s"Unknown value JSON type: ${j.getClass.simpleScalaName}", c.history))
  }

  object conversions {
    implicit def implicitNumericValue(number: Int): Value = NumberValue(number)
    implicit def implicitStringValue(string: String): Value = StringValue(string)
  }
}

final case class StringValue(string: String) extends Value {
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

object StringValue {
  @javaApi def of(value: String) = StringValue(value)
}

final case class NumberValue(number: BigDecimal) extends Value
{
  def toStringValue = Right(StringValue(number.toString))

  override def toNumber = Right(this)

  override def toBoolean =
    if (number == NumberValue.One.number) Right(BooleanValue.True)
    else
    if (number == NumberValue.Zero.number) Right(BooleanValue.False)
    else
      super.toBoolean

  def toJava = number: BigDecimal

  def convertToString = number.toString

  override def toString = convertToString
}

object NumberValue {
  val Zero = NumberValue(0)
  val One = NumberValue(1)

  def fromString(number: String): Checked[NumberValue] =
    try
      Right(NumberValue(BigDecimal(number)))
    catch { case e: NumberFormatException => Problem(Option(e.getMessage) getOrElse e.toString)}

  @javaApi def of(value: BigDecimal) = NumberValue(value)
  @javaApi def of(value: java.lang.Long) = NumberValue(BigDecimal(value))
  @javaApi def of(value: java.lang.Integer) = NumberValue(BigDecimal(value))
}

final case class BooleanValue(booleanValue: Boolean) extends Value
{
  override def toNumber =
    Right(if (booleanValue) NumberValue.One else NumberValue.Zero)

  override def toBoolean = Right(BooleanValue(booleanValue))

  def toJava = java.lang.Boolean.valueOf(booleanValue)

  def toStringValue = Right(StringValue(convertToString))

  def convertToString = booleanValue.toString

  override def toString = convertToString
}

object BooleanValue {
  val True = BooleanValue(true)
  val False = BooleanValue(false)

  @javaApi def of(value: java.lang.Boolean) = BooleanValue(value)
}

final case class ListValue(list: Seq[Value]) extends Value
{
  override def toList = Right(this)

  def toJava = list.asJava

  def toStringValue = Right(StringValue(convertToString))

  def convertToString = list.mkString("[", ", ", "]")

  override def toString = convertToString
}

object ListValue {
  @javaApi def of(values: java.util.List[Value]) = ListValue(values.asScala.toVector)
  @javaApi def of(values: Array[Value]) = ListValue(values.toVector)
}

final case class ObjectValue(nameToValue: Map[String, Value]) extends Value
{
  override def toObject = Right(this)

  def toStringValue = Right(StringValue(convertToString))

  def toJava = ???

  def convertToString = nameToValue
    .map { case (k, v) => quoteString(k) + ":" + v }
    .mkString("{", ", ", "}")

  override def toString = convertToString
}

object ObjectValue {
  @javaApi def of(values: java.util.List[Value]) = ListValue(values.asScala.toVector)
  @javaApi def of(values: Array[Value]) = ListValue(values.toVector)
}

private case class InvalidExpressionTypeProblem(typ: String, value: Value) extends Problem.Coded {
  def arguments = Map(
    "type" -> typ,
    "value" -> value.toString.truncateWithEllipsis(30))
}
