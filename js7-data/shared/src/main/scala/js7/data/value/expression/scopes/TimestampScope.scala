package js7.data.value.expression.scopes

import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.data.value.StringValue
import js7.data.value.expression.Expression.{Argument, FunctionCall}
import js7.data.value.expression.scopes.TimestampScope._
import js7.data.value.expression.{Expression, Scope}

final class TimestampScope(name: String, lazyTimestamp: => Option[Timestamp]) extends Scope
{
  private lazy val maybeTimestamp = lazyTimestamp

  override def evalFunctionCall(functionCall: Expression.FunctionCall) =
    functionCall match {
      // now(format='yyyy-MM-dd', timezone='Antarctica/Troll'
      case FunctionCall(`name`, Seq(
        Argument(formatExpr, None | Some("format")),
        Argument(timezoneExpr, None | Some("timezone")))) =>
        Some(func(formatExpr, timezoneExpr))

      case FunctionCall(`name`, Seq(Argument(formatExpr, None | Some("format")))) =>
        Some(func2(formatExpr, None))

      case _ => None
    }

  private def func(formatExpr: Expression, timezoneExpr: Expression) =
    for {
      timezoneName <- evaluator.eval(timezoneExpr).flatMap(_.toStringValueString)
      timezone = timezoneName.nonEmpty ? timezoneName
      result <- func2(formatExpr, timezone)
    } yield result

  private def func2(formatExpr: Expression, timezone: Option[String]) = {
    for {
      format <- evaluator.eval(formatExpr).flatMap(_.toStringValueString)
      result <- maybeTimestamp.fold(Checked(""))(formatTimestamp(_, format, timezone))
    } yield StringValue(result)
  }
}

object TimestampScope
{
  def apply(name: String, lazyTimestamp: => Option[Timestamp]): Scope =
    new TimestampScope(name, lazyTimestamp)

  /** JVM only */
  private def formatTimestamp(timestamp: Timestamp, format: String, maybeTimezone: Option[String])
  : Checked[String] =
    MyDateTimeFormatter.formatTimestamp(timestamp, format, maybeTimezone)
}
