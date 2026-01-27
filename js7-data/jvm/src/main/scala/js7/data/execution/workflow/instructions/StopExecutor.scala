package js7.data.execution.workflow.instructions

import js7.data.event.EventCalc
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderStopped}
import js7.data.order.{OrderEvent, OrderId}
import js7.data.state.EngineState_
import js7.data.workflow.instructions.Stop

private object StopExecutor extends EventInstructionExecutor_[Stop]:

  def toEventCalc[S <: EngineState_[S]](instr: Stop, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    detachOrder(orderId): (coll, order) =>
      start(coll, orderId): (coll, order) =>
        coll:
          orderId <-: OrderStopped
