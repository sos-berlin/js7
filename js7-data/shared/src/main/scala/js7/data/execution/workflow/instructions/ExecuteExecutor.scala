package js7.data.execution.workflow.instructions

import js7.data.execution.workflow.context.StateView
import js7.data.order.OrderEvent.{OrderAttachable, OrderFailedIntermediate_, OrderMoved, OrderProcessingKilled}
import js7.data.order.Outcome.Disrupted.JobSchedulerRestarted
import js7.data.order.{Order, Outcome}
import js7.data.workflow.instructions.Execute

private[instructions] final class ExecuteExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor
{
  type Instr = Execute

  def toEvents(instruction: Execute, order: Order[Order.State], state: StateView) =
    if (order.isProcessable && order.isDetached)
      for (job <- state.workflowJob(order.workflowPosition)) yield
        (order.id <-: OrderAttachable(job.agentPath)) :: Nil
    else
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

              case _: Outcome.NotSucceeded | _: Outcome.TimedOut =>
                OrderFailedIntermediate_()

              case _: Outcome.Killed =>
                OrderProcessingKilled
            }))
        .toList)
}
