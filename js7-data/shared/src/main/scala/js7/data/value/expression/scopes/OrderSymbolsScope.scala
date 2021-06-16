package js7.data.value.expression.scopes

import js7.data.order.Order
import js7.data.value.NumberValue
import js7.data.value.expression.Scope

final class OrderSymbolsScope(order: Order[Order.State])
extends Scope
{
  override def symbolToValue(symbol: String) =
    symbol match {
      case "catchCount" => Some(Right(NumberValue(order.workflowPosition.position.catchCount)))
      case _ => None
    }
}
