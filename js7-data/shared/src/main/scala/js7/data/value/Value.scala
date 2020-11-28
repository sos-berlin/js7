package js7.data.value

import cats.instances.vector._
import cats.syntax.traverse._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import js7.base.annotation.javaApi
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

sealed trait Value
{
  def toStringValue: Checked[StringValue]

  def toNumeric: Checked[NumericValue] =
    Left(InvalidExpressionTypeProblem("Numeric", this))

  def toBoolean: Checked[BooleanValue] =
    Left(InvalidExpressionTypeProblem("Boolean", this))

  def toList: Checked[ListValue] =
    Left(InvalidExpressionTypeProblem("List", this))

  @javaApi
  def toJava: java.lang.Object

  def convertToString: String
}
object Value
{
  //def apply(value: String) = StringValue(value)
  //def apply(value: BigDecimal) = NumericValue(value)
  //def apply(value: Long) = NumericValue(BigDecimal(value))
  //def apply(value: Int) = NumericValue(BigDecimal(value))
  //def apply(value: Boolean) = BooleanValue(value)

  @javaApi def of(value: String) = StringValue(value)
  @javaApi def of(value: BigDecimal) = NumericValue(value)
  @javaApi def of(value: java.lang.Long) = NumericValue(BigDecimal(value))
  @javaApi def of(value: java.lang.Integer) = NumericValue(BigDecimal(value))
  @javaApi def of(value: java.lang.Boolean) = BooleanValue(value)

  implicit val jsonEncoder: Encoder[Value] = {
    case StringValue(o) => Json.fromString(o)
    case NumericValue(o) => Json.fromBigDecimal(o)
    case BooleanValue(o) => Json.fromBoolean(o)
    case ListValue(values) => Json.fromValues(values map jsonEncoder.apply)
  }

  implicit val jsonDecoder: Decoder[Value] = c => {
    val j = c.value
    if (j.isString)
      Right(StringValue(j.asString.get))
    else if (j.isNumber)
      j.asNumber.get.toBigDecimal match {
        case Some(o) => Right(NumericValue(o))
        case None => Left(DecodingFailure(s"JSON number is not representable as a Java BigDecimal: $j", c.history))
      }
    else if (j.isBoolean)
      Right(BooleanValue(j.asBoolean.get))
    else if (j.isArray)
      j.asArray.get.traverse(jsonDecoder.decodeJson).map(ListValue.apply)
    else
      Left(DecodingFailure(s"Unknown value JSON type: ${j.getClass.simpleScalaName}", c.history))
  }
}

final case class StringValue(string: String) extends Value {
  def toStringValue = Right(this)

  override def toNumeric =
    try Right(NumericValue(BigDecimal(string)))
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

final case class NumericValue(number: BigDecimal) extends Value
{
  def toStringValue = Right(StringValue(number.toString))

  override def toNumeric = Right(this)

  override def toBoolean =
    if (number == NumericValue.One.number) Right(BooleanValue.True)
    else
    if (number == NumericValue.Zero.number) Right(BooleanValue.False)
    else
      super.toBoolean

  def toJava = number: BigDecimal

  def convertToString = number.toString

  override def toString = convertToString
}
object NumericValue {
  val Zero = NumericValue(0)
  val One = NumericValue(1)

  def fromString(number: String): Checked[NumericValue] =
    try
      Right(NumericValue(BigDecimal(number)))
    catch { case e: NumberFormatException => Problem(Option(e.getMessage) getOrElse e.toString)}

  @javaApi def of(value: BigDecimal) = NumericValue(value)
  @javaApi def of(value: java.lang.Long) = NumericValue(BigDecimal(value))
  @javaApi def of(value: java.lang.Integer) = NumericValue(BigDecimal(value))
}

final case class BooleanValue(booleanValue: Boolean) extends Value
{
  def toStringValue = Right(StringValue(booleanValue.toString))

  override def toNumeric =
    Right(if (booleanValue) NumericValue.One else NumericValue.Zero)

  override def toBoolean = Right(BooleanValue(booleanValue))

  def toJava = java.lang.Boolean.valueOf(booleanValue)

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
  def toStringValue = Right(StringValue(list.mkString("[", ", ", "]")))

  override def toList = Right(this)

  def toJava = list.asJava

  def convertToString = toString /*???*/

  override def toString = list.mkString("[", ", ", "]")
}
object ListValue {
  @javaApi def of(values: java.util.List[Value]) = ListValue(values.asScala.toVector)
  @javaApi def of(values: Array[Value]) = ListValue(values.toVector)
}

private case class InvalidExpressionTypeProblem(typ: String, value: Value) extends Problem.Coded {
  def arguments = Map(
    "type" -> typ,
    "value" -> value.toString.truncateWithEllipsis(30))
}
