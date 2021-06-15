package js7.data.value.expression.scopes

import js7.base.time.Timestamp
import js7.data.value.NumberValue
import js7.data.value.expression.ValueSearch.{LastOccurred, Name}
import js7.data.value.expression.{Expression, Scope, ValueSearch}

final class NowScope extends Scope
{
  lazy val now = Timestamp.now
  private val timestampScope = new TimestampScope("now", Some(now))

  override def findValue(search: ValueSearch) =
    Right(search match {
    // $epochMilli
      case ValueSearch(LastOccurred, Name("epochMilli")) =>
        Some(NumberValue(now.toEpochMilli))

      // $epochSecond
      case ValueSearch(LastOccurred, Name("epochSecond")) =>
        Some(NumberValue(now.toEpochMilli / 1000))

      case _ => None
    })

  override def evalFunctionCall(functionCall: Expression.FunctionCall) =
    timestampScope.evalFunctionCall(functionCall)
}
