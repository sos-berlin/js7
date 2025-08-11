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
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, KeyedEvent, Stamped}
import js7.data.job.JobKey
import js7.data.order.OrderEvent.{OrderProcessed, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId, OrderOutcome}
import js7.data.subagent.Problems.{SubagentIsShuttingDownProblem, SubagentShutDownBeforeProcessStartProblem}
import js7.data.subagent.SubagentCommand.{AttachSignedItem, DedicateSubagent}
import js7.data.subagent.SubagentItemStateEvent.{SubagentCouplingFailed, SubagentDedicated, SubagentEventsObserved, SubagentRestarted}
import js7.data.subagent.{SubagentCommand, SubagentDirectorState, SubagentEvent, SubagentItem, SubagentItemStateEvent, SubagentRunId}
import js7.journal.problems.Problems.JournalKilledProblem
import js7.journal.{CommitOptions, Journal}
import js7.subagent.configuration.SubagentConf
import js7.subagent.priority.ServerMeteringLiveScope
import js7.subagent.{LocalSubagentApi, Subagent}

private final class LocalSubagentDriver[S <: SubagentDirectorState[S]] private(
  // Change of disabled does not change this subagentItem.
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
  private val wasRemote = subagent.isDedicated
  protected val api = new LocalSubagentApi(subagent)
  @volatile private var _testFailover = false

  subagent.suppressJournalLogging(true) // Events are logged by the Director's Journal

  protected def isHeartbeating = true

  protected def isShuttingDown = false

  protected def start =
    dedicate.map(_.orThrow) *>
      startService:
        untilStopRequested

  private def dedicate: IO[Checked[Unit]] =
    logger.debugIO:
      IO.defer:
        if wasRemote then
          IO.right(())
        else
          val agentRunId = journal.unsafeAggregate().agentRunId
          subagent
            .executeDedicateSubagent:
              DedicateSubagent(subagentId, subagentItem.agentPath, agentRunId, controllerId)
            .flatMapT: response =>
              persistDedicated(response.subagentRunId)

  private def persistDedicated(subagentRunId: SubagentRunId): IO[Checked[Unit]] =
    journal.persist: state =>
      state.idToSubagentItemState.get(subagentId)
        .exists(_.subagentRunId.nonEmpty).thenVector:
          subagentId <-: SubagentRestarted
        .appended:
          subagentId <-: SubagentDedicated(subagentRunId, Some(currentPlatformInfo()))
    .rightAs(())

  def startObserving: IO[Unit] =
    observe.startAndForget

  // TODO Similar to SubagentEventListener
  private def observe: IO[Unit] =
    logger.debugIO:
      journal.aggregate
        .map(_.idToSubagentItemState(subagentId).eventId)
        .flatMap: eventId =>
          observeAfter(eventId).completedL

  // TODO Similar to SubagentEventListener
  private def observeAfter(eventId: EventId): fs2.Stream[IO, Unit] =
    logger.debugStream("observeAfter", eventId):
      subagent.journal.eventWatch
        .stream(EventRequest.singleClass[Event](after = eventId, timeout = None))
        // TODO handleEvent *before* commit seems to be wrong. Check returned value of handleEvent.
        .evalMap(handleEvent)
        .groupWithin(chunkSize = 1000/*!!!*/, subagentConf.eventBufferDelay)
        .evalMap: chunk =>
          val stampedEvents = chunk.toVector.flatMap(_._1)
          val followUpAll = chunk.foldMap(_._2)
          IO.whenA(stampedEvents.nonEmpty):
            stampedEvents.foldMap: stamped =>
              stamped.value match
                case KeyedEvent(subagentItem.id, SubagentItemStateEvent.SubagentShutdown |
                                                 SubagentItemStateEvent.SubagentShutdownV7) =>
                  whenSubagentShutdown.complete(()).void
                case _ => IO.unit
            .flatMap: _ =>
              val lastEventId = stampedEvents.last.eventId
              // TODO Save Stamped timestamp
              val options = CommitOptions(
                transaction = true,
                alreadyDelayed = subagentConf.eventBufferDelay)
              journal.persistKeyedEvents(options):
                stampedEvents.map(_.value)
                  :+ (subagentId <-: SubagentEventsObserved(lastEventId))
              .map(_.orThrow /*???*/)
              .*>(releaseEvents(lastEventId))
          *>
            followUpAll
        .interruptWhenF(untilStopRequested)

  /** Returns optionally the event and a follow-up io. */
  private def handleEvent(stamped: Stamped[AnyKeyedEvent])
  : IO[(Option[Stamped[AnyKeyedEvent]], IO[Unit])] =
    stamped.value match
      case keyedEvent @ KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        event match
          case _: OrderStdWritten =>
            // TODO Save Timestamp
            // FIXME Persist asynchronous!
            IO.pure(Some(stamped) -> IO.unit)

          case orderProcessed: OrderProcessed =>
            // TODO Save Timestamp
            onOrderProcessed(orderId, orderProcessed).map:
              case None => None -> IO.unit // OrderProcessed already handled
              case Some(followUp) =>
                // The followUp IO notifies OrderActor about OrderProcessed by calling `onEvents`
                Some(stamped) -> followUp

          case _ =>
            logger.error(s"Unexpected event: $keyedEvent")
            IO.pure(None -> IO.unit)

      case KeyedEvent(_: NoKey, SubagentEvent.SubagentShutdown) =>
        IO.pure:
          Some(stamped.copy(value = subagentId <-: SubagentItemStateEvent.SubagentShutdown))
            -> IO.unit

      case KeyedEvent(_: NoKey, event: SubagentEvent.SubagentItemAttached) =>
        logger.debug(event.toShortString)
        IO.pure(None -> IO.unit)

      case keyedEvent =>
        logger.error(s"Unexpected event: $keyedEvent")
        IO.pure(None -> IO.unit)

  def serverMeteringScope(): Option[ServerMeteringLiveScope.type] =
    Some(ServerMeteringLiveScope)

  def stopJobs(jobKeys: Iterable[JobKey], signal: ProcessSignal) =
    IO.defer:
      subagent.checkedDedicatedSubagent.toOption
        .fold(IO.unit):
          _.stopJobs(jobKeys, signal)

  def terminate: IO[Unit] =
    logger.traceIO:
      stop

  def tryShutdownForRemoval: IO[Unit] =
    IO.raiseError:
      new RuntimeException("tryShutdownForRemoval: The local Subagent cannot be shut down")

  /** Continue a recovered processing Order. */
  def recoverOrderProcessing(order: Order[Order.Processing]) =
    if wasRemote /*&& false ???*/ then
      // The Order may have not yet been started (only OrderProcessingStarted emitted)
      // idempotent operation:
      startOrderProcessing(order, endOfAdmissionPeriod = order.state.endOfAdmissionPeriod)
    else
      emitOrderProcessLostAfterRestart(order)
        .map(_.orThrow)
        .start
        .map(Right(_))

  def startOrderProcessing(order: Order[Order.Processing], endOfAdmissionPeriod: Option[Timestamp])
  : IO[Checked[FiberIO[OrderProcessed]]] =
    logger.traceIO("startOrderProcessing", order.id):
      requireNotStopping
        .flatMapT: _ =>
          attachItemsForOrder(order)
        .flatMap:
          case Left(problem) =>
            logger.error(s"attachItemsForOrder ${order.id}: $problem")
            val orderProcessed = OrderProcessed(OrderOutcome.Disrupted(problem))
            journal.persist:
              order.id <-: orderProcessed
            .flatMapT: _ =>
              IO.pure(orderProcessed).start.map(Right(_))

          case Right(()) =>
            startProcessingOrder2(order, endOfAdmissionPeriod)

  private def attachItemsForOrder(order: Order[Order.Processing]): IO[Checked[Unit]] =
    signableItemsForOrderProcessing(order.workflowPosition)
      // TODO Do not attach already attached Items
      //.map(_.map(_.filterNot(signed =>
      //  alreadyAttached.get(signed.value.key) contains signed.value.itemRevision)))
      .flatMapT:
        _.traverse: signedItem =>
          subagent.commandExecutor.executeCommand(
            Numbered(0, AttachSignedItem(signedItem)),
            CommandMeta.System)
        .map:
          _.map(_.rightAs(())).combineAll

  private def startProcessingOrder2(
    order: Order[Order.Processing], endOfAdmissionPeriod: Option[Timestamp])
  : IO[Checked[FiberIO[OrderProcessed]]] =
    orderToDeferred.insert(order.id, Deferred.unsafe)
      // OrderProcessed event will fulfill and remove the Deferred
      .flatMapT: deferred =>
        orderToExecuteDefaultArguments(order)
          .flatMapT: defaultArguments =>
            subagent.startOrderProcess(order, defaultArguments, endOfAdmissionPeriod)
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

        locally:
          if _testFailover && orderProcessed.outcome.isInstanceOf[OrderOutcome.Killed] then
            IO(logger.warn:
              s"Suppressed due to failover by command: ${order.id} <-: $orderProcessed")
          else
            journal.persist(order.id <-: orderProcessed)
              .orThrow
              .productR:
                deferred.complete(orderProcessed).void
        .startAndForget

  def shutdownSubagent(
    processSignal: Option[ProcessSignal] = None,
    dontWaitForDirector: Boolean = false)
  : IO[ProgramTermination] =
    subagent.shutdown(processSignal, dontWaitForDirector = dontWaitForDirector) <*
      waitForSubagentShutDownEvent

  private def waitForSubagentShutDownEvent: IO[Unit] =
    logger.debugIO:
      whenSubagentShutdown.get.logWhenMethodTakesLonger

  def killProcess(orderId: OrderId, signal: ProcessSignal): IO[Unit] =
    subagent.killProcess(orderId, signal)
      // TODO Stop postQueuedCommand loop for this OrderId
      .map(_.onProblemHandle(problem => logger.error(s"killProcess $orderId => $problem")))

  protected def emitSubagentCouplingFailed(maybeProblem: Option[Problem]): IO[Unit] =
    logger.debugIO("emitSubagentCouplingFailed", maybeProblem):
      // TODO Suppress duplicate errors
      journal.persist(_
        .idToSubagentItemState.checked(subagentId)
        .map: subagentItemState =>
          val problem = maybeProblem
            .orElse(subagentItemState.problem)
            .getOrElse(Problem.pure("decoupled"))
          (!subagentItemState.problem.contains(problem)).thenList:
            subagentId <-: SubagentCouplingFailed(problem))
      .recoverFromProblem:
        case JournalKilledProblem =>
          logger.debug("emitSubagentCouplingFailed => JournalKilledProblem")
      .map(_.orThrow)
      .void
      .onError: t =>
        // Error isn't logged until stopEventListener is called
        IO(logger.error("emitSubagentCouplingFailed => " + t.toStringWithCauses))

  protected def releaseEvents(eventId: EventId): IO[Unit] =
    enqueueCommandAndForget:
      SubagentCommand.ReleaseEvents(eventId)

  private def enqueueCommandAndForget(cmd: SubagentCommand.Queueable): IO[Unit] =
    subagent.commandExecutor.executeCommand(Numbered(0, cmd), CommandMeta.System)
      .start
      .map(_
        .joinStd
        .map(_.orThrow)
        .void
        .handleError: t =>
          logger.error(s"${cmd.toShortString} => ${t.toStringWithCauses}",
            t.nullIfNoStackTrace)
        .startAndForget/* Don't await response */)

  def testFailover(): Unit =
    _testFailover = true

  override def toString =
    s"LocalSubagentDriver(${subagentItem.pathRev})"


object LocalSubagentDriver:

  private[director] def resource[S <: SubagentDirectorState[S]](
    subagentItem: SubagentItem,
    subagent: Subagent,
    journal: Journal[S],
    controllerId: ControllerId,
    subagentConf: SubagentConf)
  : ResourceIO[LocalSubagentDriver[S]] =
    Service.resource:
      new LocalSubagentDriver(subagentItem, subagent, journal, controllerId, subagentConf)
