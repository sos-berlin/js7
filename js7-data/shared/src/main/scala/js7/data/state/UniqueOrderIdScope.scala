package js7.data.state

import java.util.{Formatter, Locale}
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.makeUnique
import js7.data.order.OrderId
import js7.data.value.expression.Expression.{Argument, FunctionCall}
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{StringValue, Value}

final class UniqueOrderIdScope(orderIds: Set[OrderId]) extends Scope:

  override def evalFunctionCall(functionCall: Expression.FunctionCall)(using fullScope: Scope)
  : Option[Checked[Value]] =
    functionCall match
      case FunctionCall("uniqueOrderId", Some(Seq(Argument(pattern, None)))) =>
        Some:
          // TODO pattern could also be a function — this requires a serializable FunctionValue

          pattern.evalAsString.flatMap: pattern =>
            makeUnique(
              pattern,
              string => orderIds(OrderId.unchecked(string)))
              .map(StringValue(_))
      case _ => None
