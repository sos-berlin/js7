package js7.data.value.expression.scopes

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, ZoneId}
import js7.data.value.expression.Expression.{Argument, FunctionCall, StringConstant}
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
        Some(nowFunc(formatExpr, StringConstant("UTC")))

      case _ => None
    }

  private def nowFunc(formatExpr: Expression, timezoneExpr: Expression) =
    for {
      format <- evaluator.eval(formatExpr).flatMap(_.toStringValueString)
      timezone <- evaluator.eval(timezoneExpr).flatMap(_.toStringValueString)
    } yield
      StringValue(
        LocalDateTime.ofInstant(now, ZoneId.of(timezone))
          .format(DateTimeFormatter.ofPattern(format)))
}

object NowScope
{
  def apply(): Scope = new NowScope
}
