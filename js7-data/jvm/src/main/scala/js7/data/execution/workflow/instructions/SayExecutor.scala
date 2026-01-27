package js7.data.execution.workflow.instructions

import js7.data.event.EventCalc
import js7.data.execution.workflow.OrderEventSource.moveOrderToNextInstruction
import js7.data.order.Order.IsFreshOrReady
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderSaid}
import js7.data.order.OrderId
import js7.data.state.EngineState_
import js7.data.workflow.instructions.Say

private object SayExecutor extends EventInstructionExecutor_[Say]:

  def toEventCalc[S <: EngineState_[S]](instr: Say, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    useOrder(orderId): (coll, order) =>
      if order.isState[IsFreshOrReady] then
        start(coll, orderId): (coll, order) =>
          for
            scope <- coll.aggregate.toOrderScope(order)
            value <- instr.what.eval(using scope)
            coll <- coll:
              order.id <-: OrderSaid(value)
            coll <- coll:
              moveOrderToNextInstruction(order)
          yield coll
      else
        coll.nix
