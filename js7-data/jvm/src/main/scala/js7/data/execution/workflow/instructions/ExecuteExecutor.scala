package js7.data.execution.workflow.instructions

import java.time.LocalDate
import js7.base.time.AdmissionTimeSchemeForJavaTime._
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.data.execution.workflow.instructions.ExecuteExecutor.orderIdToDate
import js7.data.order.Order.{IsFreshOrReady, Processed}
import js7.data.order.OrderEvent.{OrderAttachable, OrderFailedIntermediate_, OrderMoved, OrderProcessingKilled}
import js7.data.order.Outcome.Disrupted.JobSchedulerRestarted
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.state.StateView
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import scala.util.Try

private[instructions] final class ExecuteExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor with PositionInstructionExecutor
{
  type Instr = Execute
  val instructionClass = classOf[Execute]

  def toEvents(instruction: Execute, order: Order[Order.State], state: StateView) =
    for (job <- state.workflowJob(order.workflowPosition)) yield
      order
        .ifState[IsFreshOrReady].map(order =>
          if (isSkipped(job, order)) {
            // If order should start, change nextPosition function, too!
            //? order.ifState[Fresh].map(_ => OrderStarted).toList :::
              OrderMoved(order.position.increment) :: Nil
          } else
          if (order.isProcessable && order.isDetached)
            OrderAttachable(job.agentPath) :: Nil
          else
            Nil)
        .orElse(
          // Order.Ready: Execution has to be started by the caller
          //order.ifState[Order.Fresh].map(order =>
          //  OrderStarted)
          //.orElse(
          order.ifState[Processed].map(order =>
            (order.lastOutcome match {
              case Outcome.Disrupted(JobSchedulerRestarted) =>
                OrderMoved(order.position) // Repeat

              case _: Outcome.Succeeded =>
                OrderMoved(order.position.increment)

              case _: Outcome.NotSucceeded | _: Outcome.TimedOut =>
                OrderFailedIntermediate_()

              case _: Outcome.Killed =>
                OrderProcessingKilled
            }) :: Nil))
        .toList
        .flatten
        .map(order.id <-: _)

  def nextPosition(instruction: Execute, order: Order[Order.State], state: StateView) =
    for (job <- state.workflowJob(order.workflowPosition)) yield
      isSkipped(job, order) ? order.position.increment

  private def isSkipped(job: WorkflowJob, order: Order[Order.State]): Boolean =
    job.skipIfNoAdmissionForOrderDay &&
      (
        for {
          admissionTimeScheme <- job.admissionTimeScheme
          localDate <- orderIdToDate(order.id)
        } yield !admissionTimeScheme.hasPeriodForDay(localDate)
      ).getOrElse(false)
}

object ExecuteExecutor
{
  private val OrderDateRegex = "#([0-9]{4}-[0-9][0-9]-[0-9][0-9])#.*".r

  def orderIdToDate(orderId: OrderId): Option[LocalDate] =
    if (!orderId.string.startsWith("#"))
      None
    else
      orderId.string match {
        case OrderDateRegex(date) => Try(LocalDate.parse(date)).toOption
        case _ => None
      }
}
