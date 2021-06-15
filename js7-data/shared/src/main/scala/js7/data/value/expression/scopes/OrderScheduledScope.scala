package js7.data.value.expression.scopes

import js7.data.order.Order
import js7.data.value.expression.{Expression, Scope}

final class OrderScheduledScope(order: Order[Order.State])
extends Scope
{
  private val scheduledTimeScope = new TimestampScope("scheduledOrEmpty", order.scheduledFor)

  override def evalFunctionCall(functionCall: Expression.FunctionCall) =
    scheduledTimeScope.evalFunctionCall(functionCall)
}
