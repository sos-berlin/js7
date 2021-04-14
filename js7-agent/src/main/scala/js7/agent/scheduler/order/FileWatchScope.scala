package js7.agent.scheduler.order

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, ZoneId}
import java.util.regex.Matcher
import js7.agent.scheduler.order.FileWatchScope._
import js7.base.problem.Checked
import js7.data.orderwatch.OrderWatchId
import js7.data.value.expression.Expression.{Argument, FunctionCall}
import js7.data.value.expression.{Expression, Scope, ValueSearch}
import js7.data.value.{NumberValue, StringValue, Value}
import scala.util.control.NonFatal

private final class FileWatchScope(orderWatchId: OrderWatchId, matchedMatcher: Matcher)
extends Scope
{
  import ValueSearch.{LastOccurred, Name}

  lazy val now = Instant.now

  val findValue = {
    // $orderWatchId
    case ValueSearch(LastOccurred, Name("orderWatchId")) =>
      Some(StringValue(orderWatchId.string))

    // $epochMilli
    case ValueSearch(LastOccurred, Name("epochMilli")) =>
      Some(NumberValue(now.toEpochMilli))

    // $epochSecond
    case ValueSearch(LastOccurred, Name("epochSecond")) =>
      Some(NumberValue(now.toEpochMilli / 1000))

    // $0, $1, ...
    case ValueSearch(LastOccurred, Name(NumberRegex(nr))) =>
      try {
        val index = nr.toInt
        if (index >= 0 && index <= matchedMatcher.groupCount)
          Option(StringValue(matchedMatcher.group(index)))
        else
          None
      } catch { case NonFatal(_) =>
        None
      }

    case _ => None
  }

  override def evalFunctionCall(functionCall: Expression.FunctionCall): Checked[Value] =
    functionCall match {
      // now(format='yyyy-MM-dd', timezone='Antarctica/Troll'
      case FunctionCall("now", Seq(
        Argument(formatExpr, None | Some("format")),
        Argument(timezoneExpr, None | Some("timezone")))) =>
        for {
          format <- evaluator.eval(formatExpr).flatMap(_.toStringValueString)
          timezone <- evaluator.eval(timezoneExpr).flatMap(_.toStringValueString)
        } yield
          StringValue(
            LocalDateTime.ofInstant(now, ZoneId.of(timezone))
              .format(DateTimeFormatter.ofPattern(format)))

      case _ => super.evalFunctionCall(functionCall)
    }
}

object FileWatchScope
{
  private val NumberRegex = "([0-9]+)".r
}
