package js7.data.execution.workflow.instructions

import js7.data.event.EventCalc
import js7.data.order.OrderEvent.{OrderBroken, OrderCoreEvent}
import js7.data.order.OrderId
import js7.data.state.EngineState_
import js7.data.workflow.instructions.BreakOrder

private object BreakOrderExecutor extends EventInstructionExecutor_[BreakOrder]:

  def toEventCalc[S <: EngineState_[S]](instr: BreakOrder, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    EventCalc.pure:
      orderId <-: OrderBroken()
