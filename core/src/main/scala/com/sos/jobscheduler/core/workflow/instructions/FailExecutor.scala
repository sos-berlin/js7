package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderFailed, OrderStarted}
import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.Fail

/**
  * @author Joacim Zschimmer
  */
object FailExecutor extends EventInstructionExecutor {

  type Instr = Fail

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: Fail): Option[KeyedEvent[OrderActorEvent]] =
    order.ifState[Order.Fresh].map(order =>
      order.id <-: OrderStarted)
    .orElse(
      order.ifState[Order.Ready].map(order =>
        order.id <-: OrderFailed(
          instruction.returnCode match {
            case Some(returnCode) =>
              Outcome.Failed(returnCode)
            case None =>
              order.outcome match {
                case o: Outcome.NotSucceeded => o
                case _ => Outcome.Failed(ReturnCode(-1))  // ???
            }
          })))
}
