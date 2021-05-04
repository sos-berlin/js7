package js7.data.value.expression.scopes

import java.time.format.DateTimeFormatter
import java.time.{OffsetDateTime, ZoneId}
import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.data.value.StringValue
import js7.data.value.expression.Expression.{Argument, FunctionCall}
import js7.data.value.expression.{Expression, Scope}

final class TimestampScope(name: String, lazyTimestamp: => Option[Timestamp]) extends Scope
{
  private lazy val maybeInstant = lazyTimestamp.map(_.toInstant)

  override def evalFunctionCall(functionCall: Expression.FunctionCall) =
    functionCall match {
      // now(format='yyyy-MM-dd', timezone='Antarctica/Troll'
      case FunctionCall(`name`, Seq(
        Argument(formatExpr, None | Some("format")),
        Argument(timezoneExpr, None | Some("timezone")))) =>
        Some(func(formatExpr, timezoneExpr))

      case FunctionCall(`name`, Seq(Argument(formatExpr, None | Some("format")))) =>
        Some(func2(formatExpr, ZoneId.systemDefault()))

      case _ => None
    }

  private def func(formatExpr: Expression, timezoneExpr: Expression) =
    for {
      timezoneName <- evaluator.eval(timezoneExpr).flatMap(_.toStringValueString)
      zoneId = if (timezoneName.nonEmpty) ZoneId.of(timezoneName) else ZoneId.systemDefault()
      result <- func2(formatExpr, zoneId)
    } yield result

  private def func2(formatExpr: Expression, zoneId: ZoneId) =
    for {
      format <- evaluator.eval(formatExpr).flatMap(_.toStringValueString)
      result <- Checked.catchNonFatal(maybeInstant.fold("")(instant =>
        OffsetDateTime.ofInstant(instant, zoneId).format(DateTimeFormatter.ofPattern(format))))
    } yield StringValue(result)
}

object TimestampScope
{
  def apply(name: String, lazyTimestamp: => Option[Timestamp]): Scope =
    new TimestampScope(name, lazyTimestamp)
}
