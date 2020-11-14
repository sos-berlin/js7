package js7.data.value

import cats.instances.vector._
import cats.syntax.traverse._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import js7.base.annotation.javaApi
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import scala.jdk.CollectionConverters._

sealed trait Value
{
  def toNumeric: Checked[NumericValue] =
    Left(InvalidExpressionTypeProblem("Numeric", this))

  def toStringValue: Checked[StringValue] =
    Left(InvalidExpressionTypeProblem("String", this))

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
  //def apply(value: Boolean) = BooleanValue(value)
  //def apply(value: BigDecimal) = NumericValue(value)
  //def apply(value: Long) = NumericValue(BigDecimal(value))
  //def apply(value: Int) = NumericValue(BigDecimal(value))
  //def apply(value: String) = StringValue(value)

  @javaApi def of(value: java.lang.Boolean) = BooleanValue(value)
  @javaApi def of(value: BigDecimal) = NumericValue(value)
  @javaApi def of(value: java.lang.Long) = NumericValue(BigDecimal(value))
  @javaApi def of(value: java.lang.Integer) = NumericValue(BigDecimal(value))
  @javaApi def of(value: String) = StringValue(value)

  implicit val jsonEncoder: Encoder[Value] = {
    case BooleanValue(o) => Json.fromBoolean(o)
    case NumericValue(o) => Json.fromBigDecimal(o)
    case StringValue(o) => Json.fromString(o)
    case ListValue(values) => Json.fromValues(values map jsonEncoder.apply)
  }

  implicit val jsonDecoder: Decoder[Value] = c => {
    val j = c.value
    j.asBoolean match {
      case Some(o) => Right(BooleanValue(o))
      case None =>
        j.asNumber match {
          case Some(o) =>
            o.toBigDecimal match {
              case Some(o) => Right(NumericValue(o))
              case None => Left(DecodingFailure("JSON number is not representable as a Java BigDecimal", c.history))
            }
          case None =>
            j.asString match {
              case Some(o) => Right(StringValue(o))
              case None =>
                j.asArray match {
                  case Some(jsonValues) =>
                    jsonValues.traverse(jsonDecoder.decodeJson)
                      .map(o => ListValue(o))
                  case None =>
                    Left(DecodingFailure(s"Unknown value JSON type: ${j.getClass.simpleScalaName}", c.history))
                }
            }
        }
    }
  }
}

final case class BooleanValue(booleanValue: Boolean) extends Value
{
  override def toBoolean = Right(BooleanValue(booleanValue))

  def toJava = java.lang.Boolean.valueOf(booleanValue)

  def convertToString = booleanValue.toString
}
object BooleanValue {
  val True = BooleanValue(true)
  val False = BooleanValue(false)

  @javaApi def of(value: java.lang.Boolean) = BooleanValue(value)
}

final case class NumericValue(number: BigDecimal) extends Value
{
  override def toNumeric = Right(this)

  def toJava = number: BigDecimal

  def convertToString = number.toString
}
object NumericValue {
  def fromString(number: String): Checked[NumericValue] =
    try
      Right(NumericValue(BigDecimal(number)))
    catch { case e: NumberFormatException => Problem(Option(e.getMessage) getOrElse e.toString)}

  @javaApi def of(value: BigDecimal) = NumericValue(value)
  @javaApi def of(value: java.lang.Long) = NumericValue(BigDecimal(value))
  @javaApi def of(value: java.lang.Integer) = NumericValue(BigDecimal(value))
}

final case class StringValue(string: String) extends Value {
  override def toStringValue = Right(this)
  def toJava = string
  def convertToString = string
}
object StringValue {
  @javaApi def of(value: String) = StringValue(value)
}
final case class ListValue(list: Seq[Value]) extends Value
{
  override def toList = Right(this)

  def toJava = list.asJava

  def convertToString = toString

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

