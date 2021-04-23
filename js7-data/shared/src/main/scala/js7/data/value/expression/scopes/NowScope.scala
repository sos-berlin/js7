package js7.data.value.expression.scopes

import java.time.format.DateTimeFormatter
import java.time.{Instant, OffsetDateTime, ZoneId}
import js7.base.problem.Checked
import js7.data.value.expression.Expression.{Argument, FunctionCall}
import js7.data.value.expression.ValueSearch.{LastOccurred, Name}
import js7.data.value.expression.{Expression, Scope, ValueSearch}
import js7.data.value.{NumberValue, StringValue}

final class NowScope extends Scope
{
  lazy val now = Instant.now

  val findValue = {
    // $epochMilli
    case ValueSearch(LastOccurred, Name("epochMilli")) =>
      Some(NumberValue(now.toEpochMilli))

    // $epochSecond
    case ValueSearch(LastOccurred, Name("epochSecond")) =>
      Some(NumberValue(now.toEpochMilli / 1000))

    case _ => None
  }

  override def evalFunctionCall(functionCall: Expression.FunctionCall) =
    functionCall match {
      // now(format='yyyy-MM-dd', timezone='Antarctica/Troll'
      case FunctionCall("now", Seq(
        Argument(formatExpr, None | Some("format")),
        Argument(timezoneExpr, None | Some("timezone")))) =>
        Some(nowFunc(formatExpr, timezoneExpr))

      case FunctionCall("now", Seq(Argument(formatExpr, None | Some("format")))) =>
        Some(nowFunc2(formatExpr, ZoneId.systemDefault()))

      case _ => None
    }

  private def nowFunc(formatExpr: Expression, timezoneExpr: Expression) =
    for {
      timezoneName <- evaluator.eval(timezoneExpr).flatMap(_.toStringValueString)
      zoneId = if (timezoneName.nonEmpty) ZoneId.of(timezoneName) else ZoneId.systemDefault()
      result <- nowFunc2(formatExpr, zoneId)
    } yield result

  private def nowFunc2(formatExpr: Expression, zoneId: ZoneId) =
    for {
      format <- evaluator.eval(formatExpr).flatMap(_.toStringValueString)
      result <- Checked.catchNonFatal(
        OffsetDateTime.ofInstant(now, zoneId).format(DateTimeFormatter.ofPattern(format)))
    } yield StringValue(result)
}

object NowScope
{
  def apply(): Scope = new NowScope
}
