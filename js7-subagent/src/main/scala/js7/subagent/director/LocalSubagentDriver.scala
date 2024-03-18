package js7.subagent.director

import cats.effect.kernel.Deferred
import cats.effect.{FiberIO, IO, Resource}
import cats.syntax.all.*
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.fs2utils.StreamExtensions.chunkWithin
import js7.base.io.process.ProcessSignal
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixlike.MonixLikeExtensions.*
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.stream.Numbered
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.system.PlatformInfos.currentPlatformInfo
import js7.core.command.CommandMeta
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, KeyedEvent, Stamped}
import js7.data.job.JobKey
import js7.data.order.OrderEvent.{OrderProcessed, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId, Outcome}
import js7.data.subagent.Problems.{SubagentIsShuttingDownProblem, SubagentShutDownBeforeProcessStartProblem}
import js7.data.subagent.SubagentCommand.{AttachSignedItem, DedicateSubagent}
import js7.data.subagent.SubagentItemStateEvent.{SubagentCouplingFailed, SubagentDedicated, SubagentEventsObserved, SubagentRestarted}
import js7.data.subagent.{SubagentCommand, SubagentDirectorState, SubagentEvent, SubagentItem, SubagentRunId}
import js7.journal.CommitOptions
import js7.journal.state.Journal
import js7.subagent.configuration.SubagentConf
import js7.subagent.{LocalSubagentApi, Subagent}

private final class LocalSubagentDriver private(
  val subagentItem: SubagentItem,
  subagent: Subagent,
  protected val journal: Journal[? <: SubagentDirectorState[?]],
  controllerId: ControllerId,
  protected val subagentConf: SubagentConf)
extends SubagentDriver, Service.StoppableByRequest:

  private val logger = Logger.withPrefix[this.type](subagentItem.pathRev.toString)
  // isDedicated when this Director gets activated after fail-over.
  private val wasRemote = subagent.isDedicated
  protected val api = new LocalSubagentApi(subagent)
  @volatile private var shuttingDown = false
  @volatile private var _testFailover = false

  subagent.supressJournalLogging(true) // Events are logged by the Director's Journal

  protected def isHeartbeating = true

  protected def isShuttingDown = shuttingDown

  protected def start =
    dedicate.map(_.orThrow) *>
      startService(
        untilStopRequested)

  private def dedicate: IO[Checked[Unit]] =
    logger.debugIO(IO.defer(
      if wasRemote then
        IO.right(())
      else
        subagent
          .executeDedicateSubagent(
            DedicateSubagent(subagentId, subagentItem.agentPath, controllerId))
          .flatMapT(response =>
            persistDedicated(response.subagentRunId))))

  private def persistDedicated(subagentRunId: SubagentRunId): IO[Checked[Unit]] =
    journal
      .persist(state => Right(
        state.idToSubagentItemState.get(subagentId).exists(_.subagentRunId.nonEmpty)
          .thenList(subagentId <-: SubagentRestarted)
          .appended(subagentId <-: SubagentDedicated(subagentRunId, Some(currentPlatformInfo())))))
      .rightAs(())

  def startObserving: IO[Unit] =
    observe
      .handleErrorWith(t => IO:
        logger.error(s"startObserving (in background) => ${t.toStringWithCauses}"))
      .startAndForget

  // TODO Similar to SubagentEventListener
  private def observe: IO[Unit] =
    logger.debugIO(
      journal.state
        .map(_.idToSubagentItemState(subagentId).eventId)
        .flatMap(observeAfter))


  // TODO Similar to SubagentEventListener
  private def observeAfter(eventId: EventId): IO[Unit] =
    subagent.journal.eventWatch
      .stream(EventRequest.singleClass[Event](after = eventId, timeout = None))
      .evalMap(handleEvent)
      .evalMap:
        case (maybeStampedEvent, followUp) =>
          maybeStampedEvent
            .fold(IO.unit)(stampedEvent => journal
              // TODO Save Stamped timestamp
              .persistKeyedEvent(stampedEvent.value)
              .map(_.orThrow._1 /*???*/)
              // After an OrderProcessed event an DetachProcessedOrder must be sent,
              // to terminate StartOrderProcess command idempotency detection and
              // allow a new StartOrderProcess command for a next process.
              .flatTap {
                case Stamped(_, _, KeyedEvent(orderId: OrderId, _: OrderProcessed)) =>
                  subagent.commandExecutor
                    .executeCommand(
                      Numbered(0, SubagentCommand.DetachProcessedOrder(orderId)),
                      CommandMeta.System)
                    .orThrow
                case _ => IO.unit
              }
              // TODO Emit SubagentEventsObserved for a chunk of events (use fs2)
              .*>(journal.persistKeyedEvent(
                subagentId <-: SubagentEventsObserved(stampedEvent.eventId)))
              .*>(releaseEvents(stampedEvent.eventId)))
            .*>(followUp)
      .takeUntilEval(untilStopRequested)
      .completedL
      .handleError(t => logger.error(s"observeEvents => ${t.toStringWithCauses}"))

  // NEW VERSION, a little bit slower despite it should be faster, collects multiple events in a transaction, like the Controller
  // TODO Similar to SubagentEventListener
  private def observeAfterNEW(eventId: EventId): IO[Unit] =
    subagent.journal.eventWatch
      .stream(EventRequest.singleClass[Event](after = eventId, timeout = None))
      // TODO handleEvent *before* commit seems to be wrong. Check returned value of handleEvent.
      .evalMap(handleEvent)
      .chunkWithin(chunkSize = 1000/*!!!*/, subagentConf.eventBufferDelay)
      .evalMap: chunk =>
        val keyedEvents = chunk.toVector.flatMap(_._1)
        val followUpAll = chunk.traverse(_._2).void
        IO
          .whenA(keyedEvents.nonEmpty) {
            keyedEvents
              .traverse:
                case Stamped(_, _, KeyedEvent(orderId: OrderId, _: OrderProcessed)) =>
                  // After an OrderProcessed event an DetachProcessedOrder must be sent,
                  // to terminate StartOrderProcess command idempotency detection and
                  // allow a new StartOrderProcess command for a next process.
                  subagent.commandExecutor
                    .executeCommand(
                      Numbered(0, SubagentCommand.DetachProcessedOrder(orderId)),
                      CommandMeta.System)
                    .orThrow
                    .void
                case _ => IO.unit
              .flatMap: _ =>
                val lastEventId = keyedEvents.last.eventId
                // TODO Save Stamped timestamp
                val events = keyedEvents.map(_.value)
                  :+ (subagentId <-: SubagentEventsObserved(lastEventId))
                journal
                  .persistKeyedEvents(
                    events,
                    CommitOptions(
                      transaction = true,
                      alreadyDelayed = subagentConf.eventBufferDelay))
                  .map(_.orThrow /*???*/)
                  .*>(releaseEvents(lastEventId))
          }
          .*>(followUpAll)
      .takeUntilEval(untilStopRequested)
      .completedL

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
        // Ignore this, should not happen
        IO.pure(None -> IO.unit)

      case KeyedEvent(_: NoKey, event: SubagentEvent.SubagentItemAttached) =>
        logger.debug(event.toShortString)
        IO.pure(None -> IO.unit)

      case keyedEvent =>
        logger.error(s"Unexpected event: $keyedEvent")
        IO.pure(None -> IO.unit)

  private def onOrderProcessed(orderId: OrderId, orderProcessed: OrderProcessed)
  : IO[Option[IO[Unit]]] =
    orderToDeferred.remove(orderId).map:
      case None =>
        logger.error(s"Unknown Order for event: ${orderId <-: orderProcessed}")
        None

      case Some(processing) =>
        Some(processing.complete(orderProcessed).void)

  def stopJobs(jobKeys: Iterable[JobKey], signal: ProcessSignal) =
    IO(subagent.checkedDedicatedSubagent.toOption)
      .flatMap(_
        .fold(IO.unit)(_
          .stopJobs(jobKeys, signal)))

  def terminate: IO[Unit] =
    logger.traceIO(
      stop)

  def tryShutdown: IO[Unit] =
    IO.raiseError(new RuntimeException("The local Subagent cannot be shut down"))

  /** Continue a recovered processing Order. */
  def recoverOrderProcessing(order: Order[Order.Processing]) =
    logger.traceIO("recoverOrderProcessing", order.id)(
      if wasRemote then
        requireNotStopping.flatMapT(_ =>
          startOrderProcessing(order)) // TODO startOrderProcessing again ?
      else
        emitOrderProcessLost(order)
          .map(_.orThrow)
          .start
          .map(Right(_)))

  def startOrderProcessing(order: Order[Order.Processing]): IO[Checked[FiberIO[OrderProcessed]]] =
    logger.traceIO("startOrderProcessing", order.id)(
      requireNotStopping
        .flatMapT(_ => attachItemsForOrder(order))
        .flatMapT(_ => startProcessingOrder2(order)))

  private def attachItemsForOrder(order: Order[Order.Processing]): IO[Checked[Unit]] =
    signableItemsForOrderProcessing(order.workflowPosition)
      //.map(_.map(_.filterNot(signed =>
      //  alreadyAttached.get(signed.value.key) contains signed.value.itemRevision)))
      .flatMapT(_
        .traverse(signedItem =>
          subagent.commandExecutor.executeCommand(
            Numbered(0, AttachSignedItem(signedItem)),
            CommandMeta.System))
        .map(_.map(_.rightAs(())).combineAll))

  private def startProcessingOrder2(order: Order[Order.Processing])
  : IO[Checked[FiberIO[OrderProcessed]]] =
    orderToDeferred
      .insert(order.id, Deferred.unsafe)
      .flatMap:
        case Left(problem) => IO.left(problem)
        case Right(deferred) =>
          orderToExecuteDefaultArguments(order)
            .flatMapT(subagent.startOrderProcess(order, _))
            .materializeIntoChecked
            .flatMap:
              case Left(problem) =>
                orderToDeferred
                  .remove(order.id)
                  .flatMap:
                    case None =>
                      // Deferred has been removed
                      if problem != CommandDispatcher.StoppedProblem then
                        // onSubagentDied has stopped all queued StartOrderProcess commands
                        logger.warn(s"${order.id} got OrderProcessed, so we ignore $problem")
                      IO.right(())

                    case Some(deferred) =>
                      assert(deferred eq deferred)

                      val orderProcessed = OrderProcessed(
                        problem match {
                          case SubagentIsShuttingDownProblem =>
                            Outcome.processLost(SubagentShutDownBeforeProcessStartProblem)
                          case _ => Outcome.Disrupted(problem)
                        })

                      if _testFailover && orderProcessed.outcome.isInstanceOf[Outcome.Killed] then
                        IO(logger.warn(
                          s"Suppressed due to failover by command: ${order.id} <-: $orderProcessed")
                        ).start
                      else
                        journal
                          .persistKeyedEvent(order.id <-: orderProcessed)
                          .orThrow.start

              case Right(_: FiberIO[OrderProcessed]) =>
                IO.unit
            // Now wait for OrderProcessed event fulfilling Deferred
            .*>(deferred.get)
            .start
            .map(Right(_))

  def killProcess(orderId: OrderId, signal: ProcessSignal): IO[Unit] =
    subagent.killProcess(orderId, signal)
      // TODO Stop postQueuedCommand loop for this OrderId
      .map(_.onProblemHandle(problem => logger.error(s"killProcess $orderId => $problem")))

  protected def emitSubagentCouplingFailed(maybeProblem: Option[Problem]): IO[Unit] =
    logger.debugIO("emitSubagentCouplingFailed", maybeProblem)(
      // TODO Suppress duplicate errors
      journal
        .lock(subagentId)(
          journal.persist(_
            .idToSubagentItemState.checked(subagentId)
            .map { subagentItemState =>
              val problem = maybeProblem
                .orElse(subagentItemState.problem)
                .getOrElse(Problem.pure("decoupled"))
              (!subagentItemState.problem.contains(problem))
                .thenList(subagentId <-: SubagentCouplingFailed(problem))
            }))
        .map(_.orThrow)
        .void
        .handleErrorWith(t => IO.defer {
          // Error isn't logged until stopEventListener is called
          logger.error("emitSubagentCouplingFailed => " + t.toStringWithCauses)
          IO.raiseError(t)
        }))

  protected def detachProcessedOrder(orderId: OrderId): IO[Unit] =
    enqueueCommandAndForget(
      SubagentCommand.DetachProcessedOrder(orderId))

  protected def releaseEvents(eventId: EventId): IO[Unit] =
    enqueueCommandAndForget(
      SubagentCommand.ReleaseEvents(eventId))

  private def enqueueCommandAndForget(cmd: SubagentCommand.Queueable): IO[Unit] =
    subagent.commandExecutor.executeCommand(Numbered(0, cmd), CommandMeta.System)
      .start
      .map(_
        .joinStd
        .map(_.orThrow)
        .void
        .handleError(t =>
          logger.error(s"${cmd.toShortString} => ${t.toStringWithCauses}",
            t.nullIfNoStackTrace))
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
  : Resource[IO, LocalSubagentDriver] =
    Service.resource(IO {
      new LocalSubagentDriver(subagentItem, subagent, journal, controllerId, subagentConf)
    })
