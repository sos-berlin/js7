package js7.data.value.expression.scopes

import js7.base.time.Timestamp
import js7.data.value.NumberValue
import js7.data.value.expression.ValueSearch.{LastOccurred, Name}
import js7.data.value.expression.{Expression, Scope, ValueSearch}

final class NowScope extends Scope
{
  private[scopes] val now = Timestamp.now
  private lazy val timestampScope = TimestampScope("now", Some(now))

  override def findValue(search: ValueSearch)(implicit scope: Scope) =
    search match {
      // $epochMilli
      case ValueSearch(LastOccurred, Name("epochMilli")) =>
        Right(Some(NumberValue(now.toEpochMilli)))

      // $epochSecond
      case ValueSearch(LastOccurred, Name("epochSecond")) =>
        Right(Some(NumberValue(now.toEpochMilli / 1000)))

      case _ =>
        super.findValue(search)
    }

  override def evalFunctionCall(functionCall: Expression.FunctionCall)(implicit scope: Scope) =
    timestampScope.evalFunctionCall(functionCall)

  override def toString = s"NowScope($now)"
}

object NowScope
{
  def apply(): Scope =
    new NowScope
}
