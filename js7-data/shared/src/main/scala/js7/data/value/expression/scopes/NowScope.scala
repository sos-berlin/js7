package js7.data.value.expression.scopes

import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{NumberValue, Value}

final class NowScope(val now: Timestamp = Timestamp.now) extends Scope:
  private lazy val timestampScope = TimestampScope("now", Some(now))

  override lazy val nameToCheckedValue =
    case "js7EpochMilli" => Right(NumberValue(now.toEpochMilli))
    case "js7EpochSecond" => Right(NumberValue(now.toEpochMilli / 1000))

  override def evalFunctionCall(functionCall: Expression.FunctionCall)(using Scope)
  : Option[Checked[Value]] =
    timestampScope.evalFunctionCall(functionCall)

  override def toString = s"NowScope($now)"


object NowScope:
  def apply(now: Timestamp = Timestamp.now): Scope =
    new NowScope(now)
