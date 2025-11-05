package js7.data.execution.workflow.instructions

import cats.instances.either.*
import cats.instances.option.*
import cats.syntax.traverse.*
import java.time.{LocalDate, ZoneId}
import js7.base.problem.Checked
import js7.base.time.AdmissionTimeScheme
import js7.base.time.AdmissionTimeSchemeForJavaTime.*
import js7.base.time.JavaTime.extensions.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.instructions.ExecuteExecutor.*
import js7.data.order.Order.{IsFreshOrReady, Processed}
import js7.data.order.OrderEvent.{OrderFailedIntermediate_, OrderMoved, OrderProcessingKilled}
import js7.data.order.OrderObstacle.{WaitingForAdmission, jobProcessLimitReached}
import js7.data.order.OrderOutcome.Disrupted.ProcessLost
import js7.data.order.{Order, OrderId, OrderObstacle, OrderObstacleCalculator, OrderOutcome}
import js7.data.state.StateView
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentItem}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import scala.concurrent.duration.DurationInt
import scala.util.Try

private[execution] final class ExecuteExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor, PositionInstructionExecutor:

  type Instr = Execute
  val instructionClass = classOf[Execute]

  def toEvents(instruction: Execute, order: Order[Order.State], state: StateView) =
    order.ifState[IsFreshOrReady].map: order =>
      for
        workflow <- state.idToWorkflow.checked(order.workflowId)
        given ZoneId <- workflow.timeZone.toZoneId
        job <- state.workflowJob(order.workflowPosition)
        events <-
          skippedReason(order, job).map: reason =>
            // If order should start, change nextMove function, too!
            //? order.ifState[Fresh].map(_ => OrderStarted).toList :::
            Right:
              (order.id <-: OrderMoved(order.position.increment, Some(reason))) :: Nil
          .getOrElse:
            if !order.isProcessable then
              Right(Nil)
            else
              attach(order, job.agentPath)
                .getOrElse:
                  Right:
                    checkSubagentBundle(order, state) match
                      case Right(()) => Nil
                      case Left(problem) =>
                        (order.id <-: OrderFailedIntermediate_(Some(OrderOutcome.Disrupted(problem))))
                          :: Nil
      yield
        events
    .orElse:
      order.ifState[Processed].map: order =>
        val event = order.lastOutcome match
          case OrderOutcome.Disrupted(_: ProcessLost, _) =>
            OrderMoved(order.position) // Repeat

          case _: OrderOutcome.Killed =>
            OrderProcessingKilled

          case _: OrderOutcome.NotSucceeded | _: OrderOutcome.TimedOut =>
            OrderFailedIntermediate_()

          case _: OrderOutcome.IsSucceeded =>
            OrderMoved(order.position.increment)
        Right:
          (order.id <-: event) :: Nil
    .getOrElse:
      Right(Nil)

  private def checkSubagentBundle(order: Order[IsFreshOrReady], state: StateView): Checked[Unit] =
    for
      scope <- state.toOrderScope(order)
      job <- state.workflowJob(order.workflowPosition)
      maybeSubagentBundleId <- job.subagentBundleId.traverse(_
        .evalAsString(using scope)
        .flatMap(SubagentBundleId.checked))
      maybeSubagentBundle <- maybeSubagentBundleId
        .traverse(o => state
          .keyToItem(SubagentBundle)
          .checked(o)
          .orElse(state
            .keyToItem(SubagentItem)
            .checked(o.toSubagentId)))
    yield ()

  def nextMove(instruction: Execute, order: Order[Order.State], state: StateView) =
    for
      workflow <- state.idToWorkflow.checked(order.workflowId)
      given ZoneId <- workflow.timeZone.toZoneId
      job <- state.workflowJob(order.workflowPosition)
    yield
      for reason <- skippedReason(order, job) yield
        OrderMoved(order.position.increment, Some(reason))

  override def toObstacles(
    order: Order[Order.State],
    calculator: OrderObstacleCalculator)
  : Checked[Set[OrderObstacle]] =
    for
      workflow <- calculator.stateView.idToWorkflow.checked(order.workflowId)
      given ZoneId <- workflow.timeZone.toZoneId
      job <- workflow.checkedWorkflowJob(order.position)
      jobKey <- workflow.positionToJobKey(order.position)
    yield
      if order.isState[IsFreshOrReady] && !order.forceAdmission then
        val admissionObstacles = job.admissionTimeScheme
          .filterNot(_ => skippedReason(order, job).isDefined)
          .flatMap:
            _.findTimeInterval(clock.now(), limit = FindTimeIntervalLimit, dateOffset = noDateOffset)
          .map(interval => WaitingForAdmission(interval.start))
          .toSet
        admissionObstacles ++
          (calculator.jobToOrderCount(jobKey) >= job.processLimit)
            .thenSet(jobProcessLimitReached)
      else
        Set.empty


object ExecuteExecutor:

  // TODO Calendar item and PlanScheme know about order dates, too
  private val OrderDateRegex = "#([0-9]{4}-[0-9][0-9]-[0-9][0-9])#.*".r
  private[instructions] val noDateOffset = 0.s // ???
  private[instructions] val FindTimeIntervalLimit = 1096.days // TODO Use AgentConfiguration.findTimeIntervalLimit

  private def skippedReason(order: Order[Order.State], job: WorkflowJob)(using ZoneId)
  : Option[OrderMoved.Reason] =
    (!order.forceAdmission && skippedBecauseOrderDayHasNoAdmissionPeriodStart(
      order, job.admissionTimeScheme, job.skipIfNoAdmissionStartForOrderDay
    )) ? OrderMoved.NoAdmissionPeriodStart

  def skippedBecauseOrderDayHasNoAdmissionPeriodStart(
    order: Order[Order.State],
    admissionTimeScheme: Option[AdmissionTimeScheme],
    skipIfNoAdmissionStartForOrderDay: Boolean)
    (using ZoneId)
  : Boolean =
    !order.forceAdmission && skipIfNoAdmissionStartForOrderDay &&
      admissionTimeScheme.fold(false): admissionTimeScheme =>
        orderIdToDate(order.id).fold(false): localDate =>
          !admissionTimeScheme.hasAdmissionPeriodStartForDay(localDate, dateOffset = noDateOffset)

  def orderIdToDate(orderId: OrderId): Option[LocalDate] =
    orderId.string match
      case OrderDateRegex(date) => Try(LocalDate.parse(date)).toOption
      case _ => None
