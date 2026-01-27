package js7.data.execution.workflow.instructions

import cats.instances.either.*
import cats.instances.option.*
import cats.syntax.traverse.*
import java.time.{LocalDate, ZoneId}
import js7.base.problem.Checked
import js7.base.time.AdmissionTimeSchemeForJavaTime.*
import js7.base.time.JavaTime.extensions.*
import js7.base.time.ScalaTime.*
import js7.base.time.{AdmissionTimeScheme, Timestamp}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.{EventCalc, KeyedEvent}
import js7.data.execution.workflow.OrderEventSource.{fail, moveOrderToNextInstruction}
import js7.data.execution.workflow.instructions.ExecuteExecutor.*
import js7.data.order.Order.{IsFreshOrReady, Processed}
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderMoved, OrderProcessingKilled}
import js7.data.order.OrderObstacle.{WaitingForAdmission, jobProcessLimitReached}
import js7.data.order.OrderOutcome.Disrupted.ProcessLost
import js7.data.order.{Order, OrderEvent, OrderId, OrderObstacle, OrderObstacleCalculator, OrderOutcome}
import js7.data.state.EngineEventColl.extensions.workflow
import js7.data.state.{EngineState, EngineState_}
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentItem}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import scala.concurrent.duration.DurationInt
import scala.util.Try

object ExecuteExecutor extends
  EventInstructionExecutor_[Execute],
  PositionInstructionExecutor:

  // TODO Calendar item and PlanScheme know about order dates, too
  private val OrderDateRegex = "#([0-9]{4}-[0-9][0-9]-[0-9][0-9])#.*".r
  private[instructions] val noDateOffset = 0.s // ???
  private[instructions] val FindTimeIntervalLimit = 1096.days // TODO Use AgentConfiguration.findTimeIntervalLimit

  def toEventCalc[S <: EngineState_[S]](instr: Execute, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    useOrder(orderId): (coll, order) =>
      order.ifState[IsFreshOrReady].map: order =>
        coll:
          freshOrReadyEvents(instr, order)
      .orElse:
        order.ifState[Processed].map: order =>
          coll:
            processedEvents(instr, order)
      .getOrElse:
        coll.nix

  private def freshOrReadyEvents[S <: EngineState_[S]](
    instr: Execute,
    order: Order[IsFreshOrReady])
  : EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      for
        workflow <- coll.workflow(order.workflowId)
        given ZoneId <- workflow.timeZone.toZoneId
        job <- coll.aggregate.workflowJob(order.workflowPosition)
        events <-
          skippedReason(order, job).map: reason =>
            // If order should start, change nextMove function, too!
            //? order.ifState[Fresh].map(_ => OrderStarted).toList :::
            coll:
              moveOrderToNextInstruction(order, Some(reason))
          .getOrElse:
            if !order.isProcessable then
              coll.nix
            else
              coll:
                attach(order, job.agentPath):
                  EventCalc[S, OrderCoreEvent]: coll =>
                    checkSubagentBundle(order, coll.aggregate) match
                      case Left(problem) =>
                        coll:
                          fail[S](order.id, Some(OrderOutcome.Disrupted(problem)))
                      case Right(()) =>
                        coll.nix
      yield
        events

  private def processedEvents[S <: EngineState_[S]](
    instr: Execute,
    order: Order[Order.State])
  : EventCalc[S, OrderCoreEvent] =
    order.lastOutcome match
      case OrderOutcome.Disrupted(_: ProcessLost, _) =>
        EventCalc.pure:
          // Explicit OrderMoved despite unchange position, for resetting state ot to Ready
          order.id <-: OrderMoved(order.position) // Repeat

      case _: OrderOutcome.Killed =>
        EventCalc.pure:
          order.id <-: OrderProcessingKilled

      case _: OrderOutcome.NotSucceeded | _: OrderOutcome.TimedOut =>
        fail[S](order.id)

      case _: OrderOutcome.IsSucceeded =>
        moveOrderToNextInstruction(order).widen

  private def checkSubagentBundle(order: Order[IsFreshOrReady], engineState: EngineState)
  : Checked[Unit] =
    for
      scope <- engineState.toOrderScope(order)
      job <- engineState.workflowJob(order.workflowPosition)
      maybeSubagentBundleId <- job.subagentBundleId.traverse(_
        .evalAsString(using scope)
        .flatMap(SubagentBundleId.checked))
      maybeSubagentBundle <- maybeSubagentBundleId
        .traverse: o =>
          engineState.keyToItem(SubagentBundle).get(o).map(Right(_))
            .getOrElse:
              engineState.keyToItem(SubagentItem).checked(o.toSubagentId)
    yield ()

  override def nextMove(instruction: Execute, order: Order[Order.State], state: EngineState) =
    for
      workflow <- state.idToWorkflow.checked(order.workflowId)
      given ZoneId <- workflow.timeZone.toZoneId
      job <- state.workflowJob(order.workflowPosition)
    yield
      for reason <- skippedReason(order, job) yield
        OrderMoved(order.position.increment, Some(reason))

  override def toObstacles(
    order: Order[Order.State],
    calculator: OrderObstacleCalculator,
    now: Timestamp)
  : Checked[Set[OrderObstacle]] =
    for
      workflow <- calculator.engineState.idToWorkflow.checked(order.workflowId)
      given ZoneId <- workflow.timeZone.toZoneId
      job <- workflow.checkedWorkflowJob(order.position)
      jobKey <- workflow.positionToJobKey(order.position)
    yield
      if order.isState[IsFreshOrReady] && !order.forceAdmission then
        val admissionObstacles = job.admissionTimeScheme
          .filterNot(_ => skippedReason(order, job).isDefined)
          .flatMap:
            _.findTimeInterval(now, limit = FindTimeIntervalLimit, dateOffset = noDateOffset)
          .map(interval => WaitingForAdmission(interval.start))
          .toSet
        admissionObstacles ++
          (calculator.jobToOrderCount(jobKey) >= job.processLimit)
            .thenSet(jobProcessLimitReached)
      else
        Set.empty

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
