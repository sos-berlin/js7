package js7.data.execution.workflow.instructions

import js7.data.execution.workflow.context.OrderContext
import js7.data.order.OrderEvent.{OrderAttachable, OrderFailedIntermediate_, OrderMoved, OrderProcessingKilled}
import js7.data.order.Outcome.Disrupted.JobSchedulerRestarted
import js7.data.order.{Order, Outcome}
import js7.data.workflow.instructions.Execute

/**
  * @author Joacim Zschimmer
  */
object ExecuteExecutor extends EventInstructionExecutor
{
  type Instr = Execute

  def toEvents(instruction: Execute, order: Order[Order.State], context: OrderContext) =
    if (order.isProcessable && order.isDetached && order.mark.isEmpty) {
      for (job <- context.workflowJob(order.workflowPosition)) yield
        (order.id <-: OrderAttachable(job.agentId)) :: Nil
    } else
    // Order.Ready: Execution has to be started by the caller
    //order.ifState[Order.Fresh].map(order =>
    //  order.id <-: OrderStarted)
    //.orElse(
      Right(
        order.ifState[Order.Processed].map(order =>
          order.id <-:
            (order.lastOutcome match {
              case Outcome.Disrupted(JobSchedulerRestarted) =>
                OrderMoved(order.position) // Repeat

              case _: Outcome.Succeeded =>
                OrderMoved(order.position.increment)

              case _: Outcome.NotSucceeded =>
                OrderFailedIntermediate_()

              case _: Outcome.Killed =>
                OrderProcessingKilled
            }))
        .toList)
}
