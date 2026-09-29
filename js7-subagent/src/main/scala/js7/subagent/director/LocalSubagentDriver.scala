package js7.subagent.director

import cats.effect.{Deferred, FiberIO, IO, ResourceIO}
import cats.syntax.all.*
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.fs2utils.StreamExtensions.interruptWhenF
import js7.base.io.process.ProcessSignal
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixlike.MonixLikeExtensions.*
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.stream.Numbered
import js7.base.time.Timestamp
import js7.base.utils.CatsUtils.syntax.logWhenMethodTakesLonger
import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.system.PlatformInfos.currentPlatformInfo
import js7.core.command.CommandMeta
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, EventId, EventRequest, KeyedEvent, Stamped}
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId, OrderOutcome}
import js7.data.subagent.Problems.{SubagentIsShuttingDownProblem, SubagentShutDownBeforeProcessStartProblem}
import js7.data.subagent.SubagentCommand.{AttachSignedItem, DedicateSubagent}
import js7.data.subagent.SubagentItemStateEvent.{SubagentDedicated, SubagentRestarted}
import js7.data.subagent.{SubagentCommand, SubagentDirectorState, SubagentEvent, SubagentItem, SubagentItemStateEvent}
import js7.data.workflow.Workflow
import js7.journal.Journal
import js7.subagent.configuration.SubagentConf
import js7.subagent.priority.ServerMeteringLiveScope
import js7.subagent.{LocalSubagentApi, Subagent}

private final class LocalSubagentDriver[S <: SubagentDirectorState[S]] private(
  // Change of subagent.disabled does not change this subagentItem.
  // Then, it differs from the original SubagentItem
  val subagentItem: SubagentItem,
  subagent: Subagent,
  protected val journal: Journal[S],
  controllerId: ControllerId,
  protected val subagentConf: SubagentConf)
extends SubagentDriver, Service.StoppableByRequest:
  protected type State = S

  private val logger = Logger.withPrefix[this.type](subagentItem.pathRev.toString)
  private val whenSubagentShutdown = Deferred.unsafe[IO, Unit]
  // isDedicated when this Director gets activated after fail-over.
  private val wasRemoteAndDedicatedBeforeFailover = subagent.isDedicated
  protected val api = LocalSubagentApi(subagent)
  @volatile private var _testFailover = false

  subagent.suppressJournalLogging(true) // Events are logged by the Director's Journal

  protected def isHeartbeating = true

  protected def isShuttingDown = false

  protected def startService =
    dedicate.map(_.orThrow) *>
      runService:
        untilServiceStopRequested

  private def dedicate: IO[Checked[Unit]] =
    logger.debugIO:
      if wasRemoteAndDedicatedBeforeFailover then
        IO.right(())
      else
        journal.aggregate.map(_.agentRunId).flatMap: agentRunId =>
          subagent.executeDedicateSubagent:
            DedicateSubagent(subagentId, subagentItem.agentPath, agentRunId, controllerId)
          .flatMapT: response =>
            import response.subagentRunId
            journal.persist: state =>
              state.idToSubagentItemState.get(subagentId)
                .exists(_.subagentRunId.nonEmpty).thenVector:
                  subagentId <-: SubagentRestarted
                .appended:
                  subagentId <-: SubagentDedicated(subagentRunId, Some(currentPlatformInfo()))
            .rightAs(())

  def startObserving: IO[Unit] =
    journal.aggregate.map:
      _.idToSubagentItemState(subagentId).eventId
    .flatMap: eventId =>
      releaseEvents(eventId) *>
        observeAfter(eventId).completedL
          .startAndForget

  private def observeAfter(eventId: EventId): fs2.Stream[IO, Unit] =
    logger.debugStream("observeAfter", eventId):
      subagent.journal.eventWatch
        .stream(EventRequest.singleClass[Event](after = eventId, timeout = None))
        .through:
          eventHandler.pipe(bufferSize = 1000/*!!!*/)
        // FIXME Don't cancel ongoing operations above, which may not be ready for cancellation!
        .interruptWhenF(untilServiceStopRequested)

  private val eventHandler =
    SubagentEventHandler(
      subagentId, journal,
      eventDelay = subagentConf.eventBufferDelay,
      onOrderProcessed,
      // TODO releaseEvents also when no event is persisted. Use last EventId before handleEvent!
      releaseEvents
    ):
      case stamped @ Stamped(_, _, KeyedEvent(NoKey, SubagentEvent.SubagentShutdown)) =>
        whenSubagentShutdown.complete(()).as:
          Some(stamped.copy(value = subagentId <-: SubagentItemStateEvent.SubagentShutdown))
            -> IO.unit

  def serverMeteringScope(): Option[ServerMeteringLiveScope.type] =
    Some(ServerMeteringLiveScope)

  def stopWorkflowJobs(workflow: Workflow) =
    IO.defer:
      subagent.checkedDedicatedSubagent.toOption.foldMapM:
        _.stopWorkflowJobs(workflow)

  def tryShutdownForRemoval: IO[Unit] =
    IO.raiseError:
      new RuntimeException("tryShutdownForRemoval: The local Subagent cannot be shut down")

  /** Continue a recovered processing Order. */
  def recoverOrderProcessing(order: Order[Order.Processing]) =
    if wasRemoteAndDedicatedBeforeFailover then
      // The Order may have not yet been started (only OrderProcessingStarted emitted)
      // idempotent operation:
      startOrderProcessing(order, timeoutAt = order.state.timeoutAt)
    else
      emitOrderProcessLostAfterRestart(order)
        .map(_.orThrow)
        .start
        .map(Right(_))

  def startOrderProcessing(order: Order[Order.Processing], timeoutAt: Option[Timestamp])
  : IO[Checked[FiberIO[OrderProcessed]]] =
    logger.traceIO("startOrderProcessing", order.id):
      requireServiceIsNotStopping.flatMap:
        case Left(problem) =>
          persistOrderProcessed(order.id, OrderOutcome.processLostUnchecked(problem))

        case Right(()) =>
          attachItemsForOrder(order).flatMap:
            case Left(problem) =>
              logger.error(s"attachItemsForOrder ${order.id}: $problem")
              persistOrderProcessed(order.id, OrderOutcome.Disrupted(problem))

            case Right(()) =>
              startProcessingOrder2(order, timeoutAt)
                .recoverFromProblemWith:
                  case problem: SubagentIsShuttingDownProblem =>
                    persistOrderProcessed(order.id, OrderOutcome.processLostUnchecked(problem))
                  case problem =>
                    persistOrderProcessed(order.id, OrderOutcome.Failed.fromProblem(problem))

  private def persistOrderProcessed(orderId: OrderId, outcome: OrderOutcome.NotSucceeded)
  : IO[Checked[FiberIO[OrderProcessed]]] =
    journal.persistOne:
      orderId <-: OrderProcessed(outcome)
    .flatMapT: (stamped, _) =>
      IO.pure(stamped.value.event)
        .start
        .map(Right(_))

  private def attachItemsForOrder(order: Order[Order.Processing]): IO[Checked[Unit]] =
    signableItemsForOrderProcessing(order.workflowPosition)
      // TODO Do not attach already attached Items
      //.map(_.map(_.filterNot(signed =>
      //  alreadyAttached.get(signed.value.key) contains signed.value.itemRevision)))
      .flatMapT:
        _.traverse: signedItem =>
          executeCommand:
            AttachSignedItem(signedItem)
        .map:
          _.map(_.rightAs(())).combineAll

  private def startProcessingOrder2(
    order: Order[Order.Processing], timeoutAt: Option[Timestamp])
  : IO[Checked[FiberIO[OrderProcessed]]] =
    orderToDeferred.insert(order.id, Deferred.unsafe)
      // OrderProcessed event will fulfill and remove the Deferred
      .flatMapT: deferred =>
        orderToExecuteDefaultArguments(order)
          .flatMapT: defaultArguments =>
            subagent.startOrderProcess(order, defaultArguments, timeoutAt)
          .catchIntoChecked
          .recoverFromProblemWith: problem =>
            logger.trace(s"💥 startProcessingOrder2: $problem")
            startProcessingOrderFailed(order, problem)
              .map(Right(_))
          .productR:
            // Now wait for OrderProcessed event fulfilling Deferred
            deferred.get.start
          .map(Right(_))

  private def startProcessingOrderFailed(order: Order[Order.Processing], problem: Problem)
  : IO[Unit] =
    orderToDeferred.remove(order.id).flatMap:
      case None =>
        // Deferred has already been completed and removed
        IO:
          if problem != CommandDispatcher.StoppedProblem then
            // onSubagentDied has stopped all queued StartOrderProcess commands
            logger.info(s"${order.id} got OrderProcessed, so we ignore $problem")

      case Some(deferred) =>
        val orderProcessed = problem match
          case SubagentIsShuttingDownProblem =>
            OrderProcessed.processLost(SubagentShutDownBeforeProcessStartProblem)
          case _ =>
            OrderProcessed(OrderOutcome.Disrupted(problem))

        journal.persist(order.id <-: orderProcessed)
          .orThrow
          .productR:
            deferred.complete(orderProcessed).void
          .startAndForget

  def shutdownSubagent(cmd: SubagentCommand.ShutDown, meta: CommandMeta): IO[ProgramTermination] =
    subagent.shutdown(cmd, meta) <*
      waitForSubagentShutDownEvent

  private def waitForSubagentShutDownEvent: IO[Unit] =
    logger.debugIO:
      whenSubagentShutdown.get.logWhenMethodTakesLonger

  def killProcess(orderId: OrderId, signal: ProcessSignal): IO[Unit] =
    subagent.killProcess(orderId, signal)
      // TODO Stop postQueuedCommand loop for this OrderId
      .handleProblem: problem =>
        logger.error(s"killProcess $orderId => $problem")

  private def releaseEvents(eventId: EventId): IO[Unit] =
    executeCommand:
      SubagentCommand.ReleaseEvents(eventId)
    .rightAs(())
    .orThrow

  private def executeCommand(cmd: SubagentCommand): IO[Checked[SubagentCommand.Response]] =
    api.executeSubagentCommand(Numbered(0, cmd))

  override def toString =
    s"LocalSubagentDriver(${subagentItem.pathRev})"


object LocalSubagentDriver:

  private[director] def service[S <: SubagentDirectorState[S]](
    subagentItem: SubagentItem,
    subagent: Subagent,
    journal: Journal[S],
    controllerId: ControllerId,
    subagentConf: SubagentConf)
  : ResourceIO[LocalSubagentDriver[S]] =
    Service.resource:
      LocalSubagentDriver(subagentItem, subagent, journal, controllerId, subagentConf)
