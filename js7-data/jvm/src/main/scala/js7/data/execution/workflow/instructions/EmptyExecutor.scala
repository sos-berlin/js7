package js7.data.execution.workflow.instructions

import js7.data.event.EventCalc
import js7.data.execution.workflow.OrderEventSource.moveOrderToNextInstruction
import js7.data.order.Order.IsFreshOrReady
import js7.data.order.OrderEvent.OrderCoreEvent
import js7.data.order.OrderId
import js7.data.state.EngineState_
import js7.data.workflow.instructions.EmptyInstruction

private object EmptyExecutor extends EventInstructionExecutor_[EmptyInstruction]:

  def toEventCalc[S <: EngineState_[S]](instr: EmptyInstruction, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    useOrder(orderId): (coll, order) =>
      order.ifState[IsFreshOrReady].map: order =>
        coll:
          moveOrderToNextInstruction(order)
      .getOrElse:
        coll.nix
