package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.data.order.OrderEvent.{OrderFailedCatchable, OrderMoved}
import com.sos.jobscheduler.data.order.Outcome.Disrupted.JobSchedulerRestarted
import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.Execute

/**
  * @author Joacim Zschimmer
  */
object ExecuteExecutor extends EventInstructionExecutor
{
  type Instr = Execute

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: Execute) =
    Right(
      // Order.Ready: Execution has to be started by the caller
      //order.ifState[Order.Fresh].map(order =>
      //  order.id <-: OrderStarted)
      //.orElse(
        order.ifState[Order.Processed].map(order =>
          order.id <-: (
            order.lastOutcome match {
              case Outcome.Disrupted(JobSchedulerRestarted) =>
                OrderMoved(order.position) // Repeat

              case _: Outcome.Succeeded =>
                OrderMoved(order.position.increment)

              case failed: Outcome.NotSucceeded =>
                OrderFailedCatchable(failed)
            })))
}
