package js7.data.execution.workflow.instructions

import cats.instances.either.*
import cats.instances.option.*
import cats.syntax.traverse.*
import java.time.LocalDate
import js7.base.problem.Checked
import js7.base.time.AdmissionTimeSchemeForJavaTime.*
import js7.base.time.JavaTime.JavaTimeZone
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichPartialFunction}
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.instructions.ExecuteExecutor.{noDateOffset, orderIdToDate}
import js7.data.order.Order.{IsFreshOrReady, Processed}
import js7.data.order.OrderEvent.{OrderActorEvent, OrderFailedIntermediate_, OrderMoved, OrderProcessingKilled}
import js7.data.order.OrderObstacle.{WaitingForAdmission, jobProcessLimitReached}
import js7.data.order.OrderOutcome.Disrupted.ProcessLost
import js7.data.order.{Order, OrderId, OrderObstacle, OrderObstacleCalculator, OrderOutcome}
import js7.data.state.StateView
import js7.data.subagent.{SubagentItem, SubagentSelection, SubagentSelectionId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import scala.util.Try

private[execution] final class ExecuteExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor, PositionInstructionExecutor:

  type Instr = Execute
  val instructionClass = classOf[Execute]

  def toEvents(instruction: Execute, order: Order[Order.State], state: StateView) =
    order
      .ifState[IsFreshOrReady].map: order =>
        state.workflowJob(order.workflowPosition).flatMap: job =>
          skippedReason(order, job, state)
          .map: reason =>
            // If order should start, change nextMove function, too!
            //? order.ifState[Fresh].map(_ => OrderStarted).toList :::
            Right:
              (order.id <-: OrderMoved(order.position.increment, Some(reason))) :: Nil
          .getOrElse:
            order.isProcessable
              .thenMaybe:
                attach(order, job.agentPath)
              .getOrElse:
                Right:
                  checkSubagentSelection(order, state) match
                    case Right(()) => Nil
                    case Left(problem) =>
                      (order.id <-: OrderFailedIntermediate_(Some(OrderOutcome.Disrupted(problem))))
                        :: Nil
      .orElse:
        // Order.Ready: Execution has to be started by the caller
        //order.ifState[Order.Fresh].map(order =>
        //  OrderStarted)
        //.orElse(
        order
          .ifState[Processed]
          .map: order =>
            val event = order.lastOutcome match
              case OrderOutcome.Disrupted(_: ProcessLost, _) =>
                OrderMoved(order.position) // Repeat

              case _: OrderOutcome.Killed =>
                OrderProcessingKilled

              case _: OrderOutcome.NotSucceeded | _: OrderOutcome.TimedOut =>
                OrderFailedIntermediate_()

              case _: OrderOutcome.Succeeded =>
                OrderMoved(order.position.increment)
            Right:
              (order.id <-: event) :: Nil
      .getOrElse:
        Right(Nil)

  private def checkSubagentSelection(order: Order[IsFreshOrReady], state: StateView): Checked[Unit] =
    for
      scope <- state.toPureOrderScope(order)
      job <- state.workflowJob(order.workflowPosition)
      maybeSubagentSelectionId <- job.subagentSelectionId.traverse(_
        .evalAsString(scope)
        .flatMap(SubagentSelectionId.checked))
      maybeSubagentSelection <- maybeSubagentSelectionId
        .traverse(o => state
          .keyToItem(SubagentSelection)
          .checked(o)
          .orElse(state
            .keyToItem(SubagentItem)
            .checked(o.toSubagentId)))
    yield ()

  def nextMove(instruction: Execute, order: Order[Order.State], state: StateView) =
    for job <- state.workflowJob(order.workflowPosition) yield
      for reason <- skippedReason(order, job, state) yield
        OrderMoved(order.position.increment, Some(reason))

  override def toObstacles(
    order: Order[Order.State],
    calculator: OrderObstacleCalculator)
  : Checked[Set[OrderObstacle]] =
    for
      workflow <- calculator.stateView.idToWorkflow.checked(order.workflowId)
      zone <- workflow.timeZone.toZoneId
      job <- workflow.checkedWorkflowJob(order.position)
      jobKey <- workflow.positionToJobKey(order.position)
    yield
      if order.isState[IsFreshOrReady] && !order.forceJobAdmission then
        val admissionObstacles = job.admissionTimeScheme
          .filterNot(_ => skippedReason(order, job, calculator.stateView).isDefined)
          .flatMap(_
            .findTimeInterval(clock.now(), zone, dateOffset = noDateOffset))
          .map(interval => WaitingForAdmission(interval.start))
          .toSet
        admissionObstacles ++
          (calculator.jobToOrderCount(jobKey) >= job.processLimit)
            .thenSet(jobProcessLimitReached)
      else
        Set.empty

  private def skippedReason(order: Order[Order.State], job: WorkflowJob, state: StateView)
  : Option[OrderMoved.Reason] =
    isSkippedBecauseOrderDayHasNoAdmissionPeriodStart(order, job) ?
      OrderMoved.NoAdmissionPeriodStart

  private def isSkippedBecauseOrderDayHasNoAdmissionPeriodStart(
    order: Order[Order.State],
    job: WorkflowJob)
  : Boolean =
    !order.forceJobAdmission &&
      job.skipIfNoAdmissionStartForOrderDay &&
      job.admissionTimeScheme.fold(false)(admissionTimeScheme =>
        orderIdToDate(order.id)
          .fold(false): localDate =>
            !admissionTimeScheme
              .hasAdmissionPeriodStartForDay(localDate, dateOffset = noDateOffset))


object ExecuteExecutor:
  // TODO Use a Calendar ?
  private val OrderDateRegex = "#([0-9]{4}-[0-9][0-9]-[0-9][0-9])#.*".r
  private[instructions] val noDateOffset = 0.s // ???

  def orderIdToDate(orderId: OrderId): Option[LocalDate] =
    orderId.string match
      case OrderDateRegex(date) => Try(LocalDate.parse(date)).toOption
      case _ => None
