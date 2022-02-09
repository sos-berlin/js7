package js7.data_for_java.common

import cats.instances.vector._
import cats.syntax.traverse._
import java.util.Optional
import js7.base.problem.{Checked, Problem}
import js7.data.value.{BooleanValue, ErrorValue, FunctionValue, ListValue, MissingValue, NamedValues, NullValue, NumberValue, ObjectValue, StringValue, Value}
import scala.jdk.CollectionConverters._

object JavaUtils
{
  val Void: Void = null.asInstanceOf[Void]

  // TEST IS MISSING
  private def javaToNamedValues(javaMap: java.util.Map[String, java.lang.Object]): Checked[NamedValues] =
    javaMap.asScala
      .view
      .map { case (k, v) => javaToValue(v).map(k -> _) }
      .toVector
      .sequence
      .map(_.toMap)

  private def javaToValue(any: java.lang.Object): Checked[Value] =
    (any: @unchecked) match {
      case o: java.lang.Boolean => Right(BooleanValue(o))
      case o: java.lang.Integer => Right(NumberValue(o.intValue))
      case o: java.lang.Long => Right(NumberValue(o.longValue))
      case o: java.math.BigDecimal => Right(NumberValue(o))
      case o: String => Right(StringValue(o))
      case values: java.util.List[_] =>
        values.asInstanceOf[java.util.List[AnyRef]]
          .asScala
          .toVector
          .traverse(javaToValue)
          .map(ListValue.apply)
      case o => Left(Problem(s"Invalid Value Java type: ${o.getClass.getName}"))
    }

  // TEST IS MISSING
  private def namedValuesToJava(namedValues: NamedValues): java.util.Map[String, java.lang.Object] =
    namedValues.view.mapValues(valueToJava).toMap.asJava

  private def valueToJava(value: Value): java.lang.Object =
    value match {
      case BooleanValue(o) => java.lang.Boolean.valueOf(o)
      case NumberValue(o) => o
      case StringValue(o) => o
      case ListValue(values) => values.asJava
      case NullValue => Optional.empty
      case _: ObjectValue | _: ErrorValue | _: FunctionValue | _: MissingValue => throw new NotImplementedError
    }
}
