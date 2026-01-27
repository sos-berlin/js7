package js7.data.execution.workflow.instructions

import js7.base.problem.Problem
import js7.data.event.EventCalc
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderDetachable}
import js7.data.order.{OrderEvent, OrderId}
import js7.data.state.EngineState_
import js7.data.workflow.instructions.Gap

object GapExecutor extends EventInstructionExecutor_[Gap]:

  def toEventCalc[S <: EngineState_[S]](instr: Gap, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    useOrder(orderId): (coll, order) =>
      if !order.isAttached then
        Left(Problem.pure(s"Gap instruction but order is not attached to an agent: $order"))
      else
        coll:
          order.id <-: OrderDetachable
