package js7.data.value.expression.scopes

import js7.data.order.Order
import js7.data.value.expression.{Expression, Scope}

final class OrderScope(order: Order[Order.Processing]) extends Scope
{
  private val timestampScope = new TimestampScope("scheduledOrEmpty", order.scheduledFor)

  override def evalFunctionCall(functionCall: Expression.FunctionCall) =
    timestampScope.evalFunctionCall(functionCall)
}

object OrderScope
{
  def apply(order: Order[Order.Processing]): Scope =
    new OrderScope(order)
}
