package js7.data.value.expression.scopes

import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{NumberValue, Value}
import scala.collection.MapView

final class NowScope(val now: Timestamp = Timestamp.now) extends Scope
{
  private lazy val timestampScope = TimestampScope("now", Some(now))

  override lazy val nameToCheckedValue =
    new MapView[String, Checked[Value]] {
      def get(key: String) = key match {
        case "js7EpochMilli" => Some(Right(NumberValue(now.toEpochMilli)))
        case "js7EpochSecond" => Some(Right(NumberValue(now.toEpochMilli / 1000)))
        case _ => None
      }

      def iterator =
        Iterator("js7EpochMilli", "js7EpochSecond")
          .flatMap(k => get(k).map(k -> _))
    }

  override def evalFunctionCall(functionCall: Expression.FunctionCall)(implicit scope: Scope) =
    timestampScope.evalFunctionCall(functionCall)

  override def toString = s"NowScope($now)"
}

object NowScope
{
  def apply(now: Timestamp = Timestamp.now): Scope =
    new NowScope(now)
}
