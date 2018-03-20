package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderMoved, OrderStopped}
import com.sos.jobscheduler.data.order.Outcome.Disrupted.JobSchedulerRestarted
import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.data.workflow.OrderContext
import com.sos.jobscheduler.data.workflow.instructions.Job

/**
  * @author Joacim Zschimmer
  */
object JobExecutor extends EventInstructionExecutor {

  type Instr = Job

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: Job): Option[KeyedEvent[OrderActorEvent]] =
    // Order.Ready: Job start has to be done by the caller
    for (order ← order.ifState[Order.Processed]) yield
      order.id <-: (
        order.state.outcome match {
          case Outcome.Disrupted(JobSchedulerRestarted) ⇒
            OrderMoved(order.position)  // Repeat

          case _: Outcome.Succeeded ⇒
            OrderMoved(order.position.increment)

          case failed: Outcome.NotSucceeded ⇒
            OrderStopped(failed)
        })
}
