package js7.subagent.director

import cats.effect.IO
import cats.syntax.applicativeError.*
import cats.syntax.option.*
import fs2.Stream
import js7.base.catsutils.CatsEffectExtensions.left
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.Switch
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.common.http.{PekkoHttpClient, RecouplingStreamReader}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, KeyedEvent, NonPersistentEvent, Stamped}
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.OrderId
import js7.data.subagent.Problems.{ProcessLostDueToShutdownProblem, ProcessLostProblem}
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentDied, SubagentShutdown}
import js7.data.subagent.SubagentState.keyedEventJsonCodec
import js7.data.subagent.{SubagentDirectorState, SubagentEvent, SubagentId, SubagentRunId}
import js7.data.system.ServerMeteringEvent
import js7.data.value.expression.Scope
import js7.journal.Journal
import scala.concurrent.duration.Deadline

private final class RemoteSubagentEventListener(
  subagentId: SubagentId,
  conf: RemoteSubagentDriver.Conf,
  recouplingStreamReaderConf: RecouplingStreamReaderConf,
  api: HttpSubagentApi,
  journal: Journal[? <: SubagentDirectorState[?]],
  enqueueReleaseEventsCommand: EventId => IO[Unit],
  onOrderProcessed: (OrderId, OrderProcessed) => IO[Option[IO[Unit]]],
  onSubagentDied: (ProcessLostProblem, SubagentDied) => IO[Unit],
  dedicateOrCouple: IO[Checked[(SubagentRunId, EventId)]],
  emitSubagentCouplingFailed: Option[Problem] => IO[Unit],
  isCoupled: () => Boolean,
  untilServiceStopRequested: IO[Unit])
extends
  Service.StoppableByCancel:

  private val logger = Logger.withPrefix[RemoteSubagentEventListener](subagentId.toString)
  private val _isHeartbeating = Atomic(false)

  private var _lastServerMeteringEvent = ServerMeteringEvent(None, 0, 0, 0)
  private var _lastServerMeteringEventSince = Deadline.now - 24.h

  private final val coupled = Switch(false)

  private val eventHandler =
    SubagentEventHandler(
      subagentId, journal, eventDelay = conf.subagentConf.eventBufferDelay max conf.commitDelay,
      onOrderProcessed, enqueueReleaseEventsCommand
    ):
      case Stamped(_, _, KeyedEvent(NoKey, e: ServerMeteringEvent)) =>
        IO:
          _lastServerMeteringEvent = e
          _lastServerMeteringEventSince = Deadline.now
          None -> IO.unit

      case Stamped(_, _, KeyedEvent(NoKey, SubagentEvent.SubagentShutdown)) =>
        // TODO Aufträge im Zustand Processing abbrechen.
        // Das sind Aufträge, für die ein OrderProcessingStarted ausgegeben wurde, die aber noch
        // nicht zum Subagenten geschickt worden sind, sodass der die nicht mit Disrupted
        // abschließen kann.
        // onSubagentDied? OrderProcessed wie oben behandeln als käme es vom Subagenten.
        IO.pure(None -> onSubagentDied(ProcessLostDueToShutdownProblem, SubagentShutdown))

  protected def startService =
    runService:
      observeEvents

  private def observeEvents: IO[Unit] =
    Stream.suspend:
      val recouplingStreamReader = newRecouplingStreamReader()
      val after = journal.unsafeAggregate().idToSubagentItemState(subagentId).eventId
      recouplingStreamReader.stream(api, after = after)
        .through:
          eventHandler.pipe(conf.subagentConf.eventBufferSize)
        .onFinalize:
          recouplingStreamReader.terminateAndLogout
            .logWhenItTakesLonger("recouplingStreamReader.terminateAndLogout")
    .compile.drain

  private def newRecouplingStreamReader() =
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
            .map(Right(_))

      override protected def onCouplingFailed(api: HttpSubagentApi, problem: Problem) =
        IO.defer:
          if isServiceStopping then
            IO.pure(false)
          else
            onSubagentDecoupled(Some(problem)) *>
              IO:
                if lastProblem contains problem then
                  logger.debug(s"⚠️  Coupling failed again: $problem")
                else
                  lastProblem = Some(problem)
                  logger.warn(s"Coupling failed: $problem")
                true

      override protected val onDecoupled =
        logger.traceIO:
          onSubagentDecoupled(None) *>
            coupled.switchOff.as(Completed)

      protected def stopRequested = false

  private def onHeartbeatStarted: IO[Unit] =
    IO.defer:
      val wasHeartbeating = _isHeartbeating.getAndSet(true)
      if !wasHeartbeating then logger.trace("_isHeartbeating := true")
      IO.whenA(!wasHeartbeating && isCoupled()):
        // Different to AgentDriver,
        // for Subagents, the Coupling state is tied to the continuous flow of events.
        journal.persist(subagentId <-: SubagentCoupled)
          .map(_.orThrow)

  def isHeartbeating = _isHeartbeating.get()

  def serverMeteringScope(): Option[Scope] =
    val latest = _lastServerMeteringEventSince + conf.heartbeatTiming.heartbeatValidDuration
    !latest.hasElapsed ? _lastServerMeteringEvent.toScope

  private def onSubagentDecoupled(problem: Option[Problem]): IO[Unit] =
    IO.defer:
      if _isHeartbeating.getAndSet(false) then logger.trace("_isHeartbeating := false")
      // We don't bother a coupling problem when we no longer listen.
      // And don't emit an event when shutting down (then we don't listen), because
      // the journal may already be unusable.
      if isServiceStopping then
        IO(logger.debug(s"onSubagentDecoupled $problem"))
      else
        emitSubagentCouplingFailed(problem)

  override def toString = s"RemoteSubagentEventListener($subagentId)"
