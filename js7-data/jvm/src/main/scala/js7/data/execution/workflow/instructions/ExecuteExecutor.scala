package js7.data.execution.workflow.instructions

import java.time.LocalDate
import js7.base.problem.Checked
import js7.base.time.AdmissionTimeSchemeForJavaTime.JavaAdmissionTimeSchemeJava
import js7.base.time.JavaTime.JavaTimeZone
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichPartialFunction}
import js7.data.execution.workflow.instructions.ExecuteExecutor.{noDateOffset, orderIdToDate}
import js7.data.order.Order.{IsFreshOrReady, Processed}
import js7.data.order.OrderEvent.{OrderAttachable, OrderFailedIntermediate_, OrderMoved, OrderProcessingKilled}
import js7.data.order.OrderObstacle.WaitingForTime
import js7.data.order.Outcome.Disrupted.JobSchedulerRestarted
import js7.data.order.{Order, OrderId, OrderObstacle, Outcome}
import js7.data.state.StateView
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import scala.util.Try

private[execution] final class ExecuteExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor with PositionInstructionExecutor
{
  type Instr = Execute
  val instructionClass = classOf[Execute]

  def toEvents(instruction: Execute, order: Order[Order.State], state: StateView) =
    for (job <- state.workflowJob(order.workflowPosition)) yield
      order
        .ifState[IsFreshOrReady].map(order =>
          if (isSkipped(order, job)) {
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
      isSkipped(order, job) ? order.position.increment

  override def toObstacles(order: Order[Order.State], state: StateView)
  : Checked[Set[OrderObstacle]] =
    for {
      workflow <- state.idToWorkflow.checked(order.workflowId)
      zone <- workflow.timeZone.toZoneId
      job <- workflow.checkedWorkflowJob(order.position)
    } yield
      job.admissionTimeScheme
        .filterNot(_ => isSkipped(order, job))
        .flatMap(_
          .findTimeInterval(clock.now(), zone, dateOffset = noDateOffset))
        .map(interval => WaitingForTime(interval.start))
        .toSet

  private def isSkipped(order: Order[Order.State], job: WorkflowJob)
  : Boolean =
    job.skipIfNoAdmissionForOrderDay &&
      job.admissionTimeScheme.fold(false)(admissionTimeScheme =>
        orderIdToDate(order.id)
          .fold(false)(localDate =>
            !admissionTimeScheme.hasPeriodForDay(localDate, dateOffset = noDateOffset)))
}

object ExecuteExecutor
{
  // TODO Use a Calendar ?
  private val OrderDateRegex = "#([0-9]{4}-[0-9][0-9]-[0-9][0-9])#.*".r
  private[instructions] val noDateOffset = 0.s // ???

  def orderIdToDate(orderId: OrderId): Option[LocalDate] =
    orderId.string match {
      case OrderDateRegex(date) => Try(LocalDate.parse(date)).toOption
      case _ => None
    }
}
