package js7.subagent.director

import cats.effect.{FiberIO, IO}
import cats.syntax.applicativeError.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import fs2.Stream
import fs2.concurrent.SignallingRef
import js7.base.catsutils.CatsEffectExtensions.{joinStd, left}
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.fs2utils.StreamExtensions.+:
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.Switch
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.pureFiberIO
import js7.base.utils.CatsUtils.syntax.{logWhenItTakesLonger, logWhenMethodTakesLonger}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{AsyncLock, Atomic}
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.common.http.{PekkoHttpClient, RecouplingStreamReader}
import js7.data.delegate.DelegateCouplingState
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, KeyedEvent, NonPersistentEvent, Stamped}
import js7.data.order.OrderEvent.{OrderProcessed, OrderStdWritten}
import js7.data.order.{OrderEvent, OrderId}
import js7.data.subagent.Problems.{ProcessLostDueToShutdownProblem, ProcessLostProblem}
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentDied, SubagentEventsObserved, SubagentShutdown}
import js7.data.subagent.SubagentState.keyedEventJsonCodec
import js7.data.subagent.{SubagentDirectorState, SubagentEvent, SubagentId, SubagentRunId}
import js7.data.system.ServerMeteringEvent
import js7.data.value.expression.Scope
import js7.journal.{CommitOptions, Journal}
import scala.concurrent.duration.Deadline
import scala.util.chaining.scalaUtilChainingOps
import scala.util.control.NonFatal

private trait SubagentEventListener:

  protected def subagentId: SubagentId
  protected def conf: RemoteSubagentDriver.Conf
  protected def recouplingStreamReaderConf: RecouplingStreamReaderConf
  protected def api: HttpSubagentApi
  protected def journal: Journal[? <: SubagentDirectorState[?]]
  protected def detachProcessedOrder(orderId: OrderId): IO[Unit]
  protected def releaseEvents(eventId: EventId): IO[Unit]
  protected def onOrderProcessed(orderId: OrderId, orderProcessed: OrderProcessed)
  : IO[Option[IO[Unit]]]
  protected def onSubagentDied(orderProblem: ProcessLostProblem, subagentDiedEvent: SubagentDied)
  : IO[Unit]
  protected def dedicateOrCouple: IO[Checked[(SubagentRunId, EventId)]]
  protected def emitSubagentCouplingFailed(maybeProblem: Option[Problem]): IO[Unit]
  protected def isCoupled: Boolean
  protected def isLocal: Boolean
  protected def untilStopRequested: IO[Unit]

  private val logger = Logger.withPrefix[SubagentEventListener](subagentId.toString)
  private val stopObserving = memoize(SignallingRef[IO].of(false))
  @volatile private var observing: FiberIO[Unit] = pureFiberIO(())
  private val _isHeartbeating = Atomic(false)
  private val isListening = Atomic(false)
  private val lock = AsyncLock()

  private var _lastServerMeteringEvent = ServerMeteringEvent(None, 0, 0, 0)
  private var _lastServerMeteringEventSince = Deadline.now - 24.h

  protected final val coupled = Switch(false)

  protected final def stopEventListener: IO[Unit] =
    lock.lock:
      logger.debugIO:
        IO.defer(IO.whenA(isListening.getAndSet(false)):
          stopObserving
            .flatMap(_.set(true))
            .*>(IO.defer:
              observing.joinStd))
          .logWhenMethodTakesLonger

  protected final def startEventListener: IO[Unit] =
    lock.lock:
      IO.defer:
        if isListening.getAndSet(true) then
          val msg = "Duplicate startEventListener"
          logger.error(msg)
          IO.raiseError(new RuntimeException(s"$toString: $msg"))
        else
          stopObserving.flatMap(_.set(false))
            .productR:
              stopObserving.flatMap:
                _.getAndDiscreteUpdates.use: (o, stream) =>
                  observeEvents(stopRequested = o +: stream)
            .onError(t => IO:
              // We have a problem
              logger.error(s"observeEvents failed: ${t.toStringWithCauses}"))
            .start
            .flatMap(fiber => IO:
              observing = fiber)

  private def observeEvents(stopRequested: Stream[IO, Boolean]): IO[Unit] =
    logger.debugStream:
      Stream.suspend:
        val recouplingStreamReader = newEventListener()
        val bufferDelay = conf.eventBufferDelay max conf.commitDelay
        val after = journal.unsafeAggregate().idToSubagentItemState(subagentId).eventId
        recouplingStreamReader.stream(api, after = after)
          .interruptWhen(stopRequested)
          .pipe: stream =>
            if !bufferDelay.isPositive then
              stream.chunks
            else
              stream.groupWithin(conf.eventBufferSize, bufferDelay)
          .evalTap(_
            .traverse(handleEvent)
            .flatMap: updatedStampedChunk0 =>
              val (updatedStampedSeqSeq, followUps) = updatedStampedChunk0.toArraySeq.unzip
              val updatedStampedSeq = updatedStampedSeqSeq.flatten
              val lastEventId = updatedStampedSeq.lastOption.map(_.eventId)
              val events = updatedStampedSeq.view.map(_.value)
                .concat(lastEventId.map:
                  subagentId <-: SubagentEventsObserved(_))
                .toVector
              // TODO Save Stamped timestamp
              journal
                .persistKeyedEvents(CommitOptions(transaction = true))(events)
                .map(_.orThrow /*???*/)
                // After an OrderProcessed event a DetachProcessedOrder must be sent,
                // to terminate StartOrderProcess command idempotency detection and
                // allow a new StartOrderProcess command for a next process.
                .*>(updatedStampedSeq
                  .collect:
                    case Stamped(_, _, KeyedEvent(o: OrderId, _: OrderProcessed)) => o
                  .traverse(detachProcessedOrder))
                .*>(lastEventId.traverse(releaseEvents))
                .*>(followUps.combineAll))
          .onFinalize:
            recouplingStreamReader.terminateAndLogout
              .logWhenItTakesLonger("recouplingStreamReader.terminateAndLogout")
    .compile.drain

  /** Returns optionally the event and a follow-up task. */
  private def handleEvent(stamped: Stamped[AnyKeyedEvent])
  : IO[(Option[Stamped[AnyKeyedEvent]], IO[Unit])] =
    stamped.value match
      case keyedEvent @ KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        event match
          case _: OrderStdWritten =>
            // TODO Save Timestamp
            IO.pure(Some(stamped) -> IO.unit)

          case orderProcessed: OrderProcessed =>
            // TODO Save Timestamp
            onOrderProcessed(orderId, orderProcessed).map:
              case None => None -> IO.unit  // OrderProcessed already handled
              case Some(followUp) =>
                // The followUp IO notifies OrderActor about OrderProcessed by calling `onEvents`
                Some(stamped) -> followUp

          case _ =>
            logger.error(s"Unexpected event: $keyedEvent")
            IO.pure(None -> IO.unit)

      case KeyedEvent(_: NoKey, e: ServerMeteringEvent) =>
        IO:
          _lastServerMeteringEvent = e
          _lastServerMeteringEventSince = Deadline.now
          None -> IO.unit

      case KeyedEvent(_: NoKey, SubagentEvent.SubagentShutdown) =>
        IO.pure(None -> onSubagentDied(ProcessLostDueToShutdownProblem, SubagentShutdown))

      case KeyedEvent(_: NoKey, event: SubagentEvent.SubagentItemAttached) =>
        logger.debug(event.toShortString)
        IO.pure(None -> IO.unit)

      case keyedEvent =>
        logger.error(s"Unexpected event: $keyedEvent")
        IO.pure(None -> IO.unit)

  private def newEventListener() =
    new RecouplingStreamReader[EventId, Stamped[AnyKeyedEvent], HttpSubagentApi](
      toIndex = stamped => !stamped.value.event.isInstanceOf[NonPersistentEvent] ? stamped.eventId,
      recouplingStreamReaderConf):

      private var lastProblem: Option[Problem] = None

      override protected def couple(eventId: EventId) =
        logger.debugIO:
          dedicateOrCouple
            .flatMapT: (_, eventId) =>
              coupled.switchOn
                .as(Right(eventId))
          .<*(IO:
            lastProblem = None)

      protected def getStream(api: HttpSubagentApi, after: EventId) =
        logger.debugIO("getStream", s"after=$after"):
          journal.aggregate
            .map(_.idToSubagentItemState.checked(subagentId).map(_.subagentRunId))
            .flatMapT:
              case None => IO.left(Problem.pure("Subagent not yet dedicated"))
              case Some(subagentRunId) => getStream(api, after, subagentRunId)

      private def getStream(api: HttpSubagentApi, after: EventId, subagentRunId: SubagentRunId) =
        api.login(onlyIfNotLoggedIn = true) *>
          api
            .eventStream(
              EventRequest.singleClass[Event](after = after, timeout = None),
              subagentRunId,
              serverMetering = conf.heartbeatTiming.heartbeat.some,
              idleTimeout = idleTimeout)
            .map(_
              .evalTap: _ =>
                onHeartbeatStarted
              .recoverWith:
                case _: PekkoHttpClient.IdleTimeoutException =>
                  Stream.exec(IO.defer:
                    val problem = Problem.pure(s"Missing heartbeat from $subagentId")
                    logger.warn(problem.toString)
                    onSubagentDecoupled(problem.some))
              .onFinalize:
                onSubagentDecoupled(problem = None)) // Since v2.7
              //.guaranteeCase(exitCase => IO.defer {
              //  // guaranteeCase runs concurrently, maybe with onDecoupled ?
              //  IO.when((exitCase != ExitCase.Completed /*&& exitCase != ExitCase.Canceled*/) || /*isCoupled*/isHeartbeating)(
              //    stopObserving.flatMap(_.tryRead).map(_.isDefined)
              //      .flatMap(stopped =>
              //        if (stopped) {
              //          logger.debug("stopped")
              //          IO.unit
              //        } else {
              //          val problem = exitCase match {
              //            case ExitCase.Completed =>
              //              Problem.pure(s"$subagentId event stream has ended")
              //            case _ =>
              //              Problem.pure(s"$subagentId event stream: $exitCase")
              //          }
              //          logger.warn(problem.toString)
              //          onSubagentDecoupled(Some(problem))
              //        }))
              //}))
            .map(Right(_))

      override protected def onCouplingFailed(api: HttpSubagentApi, problem: Problem) =
        stopObserving.flatMap(_.get)
          .flatMap: stopped =>
            if stopped then
              IO.pure(false)
            else
              onSubagentDecoupled(Some(problem)) *>
                IO:
                  if lastProblem contains problem then
                    logger.debug(s"⚠️ Coupling failed again: $problem")
                  else
                    lastProblem = Some(problem)
                    logger.warn(s"Coupling failed: $problem")
                  true

      override protected val onDecoupled =
        logger.traceIO:
          onSubagentDecoupled(None) *>
            coupled.switchOff.as(Completed)

      protected def stopRequested = false

  protected def isStopping: Boolean // Since v2.7
  protected def isShuttingDown: Boolean // Since v2.7

  private def onHeartbeatStarted: IO[Unit] =
    IO.defer:
      val wasHeartbeating = _isHeartbeating.getAndSet(true)
      if !wasHeartbeating then logger.trace("_isHeartbeating := true")
      IO.whenA(!wasHeartbeating && isCoupled):
        // Different to AgentDriver,
        // for Subagents, the Coupling state is tied to the continuous flow of events.
        journal.persist(subagentId <-: SubagentCoupled)
          .map(_.orThrow)

  private def isCouplingStateCoupled: Boolean =
    journal.unsafeAggregate().idToSubagentItemState(subagentId).couplingState ==
      DelegateCouplingState.Coupled

  protected final def isHeartbeating = isLocal || _isHeartbeating.get()

  final def serverMeteringScope(): Option[Scope] =
    try
      val latest = _lastServerMeteringEventSince + conf.heartbeatTiming.heartbeatValidDuration
      !latest.hasElapsed thenSome:
        _lastServerMeteringEvent.toScope
    catch case NonFatal(t) =>
      logger.error(s"serverMeteringScope => ${t.toStringWithCauses}")
      None

  private def onSubagentDecoupled(problem: Option[Problem]): IO[Unit] =
    IO.defer:
      if _isHeartbeating.getAndSet(false) then logger.trace("_isHeartbeating := false")
      // We don't bother a coupling problem when we no longer listen.
      // And don't emit an event when shutting down (then we don't listen), because
      // the journal may already be unusable.
      if !isListening.get() then
        IO(logger.debug(s"onSubagentDecoupled $problem"))
      else
        IO.whenA(true || isCoupled):
          emitSubagentCouplingFailed(problem)
