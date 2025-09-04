package js7.controller.agent

import cats.effect.{Deferred, IO, ResourceIO}
import cats.syntax.applicativeError.*
import cats.syntax.option.*
import fs2.Stream
import js7.agent.client.AgentClient
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.CoupleController
import js7.agent.data.event.AgentEvent
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.fs2utils.StreamExtensions.interruptWhenF
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixlike.MonixLikeExtensions.{completedL, flatMapLoop, raceMerge}
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.CatsUtils.syntax.{logWhenItTakesLonger, logWhenMethodTakesLonger}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{AsyncLock, Atomic}
import js7.common.http.{PekkoHttpClient, RecouplingStreamReader}
import js7.controller.agent.AgentDriver.DecoupledProblem
import js7.controller.agent.DirectorDriver.*
import js7.data.agent.AgentRefStateEvent.{AgentCoupled, AgentReset}
import js7.data.agent.Problems.AgentNotDedicatedProblem
import js7.data.agent.{AgentPath, AgentRef, AgentRefState, AgentRunId}
import js7.data.cluster.ClusterEvent
import js7.data.controller.{ControllerRunId, ControllerState}
import js7.data.delegate.DelegateCouplingState.{Coupled, Resetting}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, Stamped}
import js7.data.item.BasicItemEvent.ItemAttachable
import js7.data.item.InventoryItemEvent
import js7.data.order.{OrderEvent, OrderId}
import js7.data.orderwatch.OrderWatchEvent
import js7.data.subagent.SubagentItemStateEvent
import js7.journal.Journal
import scala.util.chaining.scalaUtilChainingOps

private[agent] final class DirectorDriver private(
  agentDriver: AgentDriver,
  agentPath: AgentPath,
  initialEventId: EventId,
  client: AgentClient,
  dedicateAgentIfNeeded: DirectorDriver => IO[Checked[(AgentRunId, EventId)]],
  onCouplingFailed_ : Problem => IO[Boolean],
  onCoupled_ : Set[OrderId] => IO[Unit],
  onDecoupled_ : IO[Unit],
  adoptEvents: Seq[Stamped[AnyKeyedEvent]] => IO[Unit],
  journal: Journal[ControllerState],
  conf: AgentDriverConfiguration)
extends Service.StoppableByRequest:
  directorDriver =>

  private val index = DirectorDriver.index.incrementAndGet()
  private val logger = Logger.withPrefix[this.type](agentPath.toString + " #" + index)
  private var adoptedEventId = initialEventId
  private val untilFetchingStopped = Deferred.unsafe[IO, Unit]
  private val onFetchedEventsLock = AsyncLock() // Fence for super.isStopping

  logger.trace(s"initialEventId=$initialEventId")

  protected def start =
    startService(
      (untilStopRequested *> eventFetcher.stopStreaming)
        .background.surround:
          continuallyFetchEvents *>
            IO(assertThat(isStopping)) *>
            //? May block switch-over when JournalActor does not respond to Events (due to
            // switch-over).
            //? onFetchedEventsLock.lock(IO.unit) *>
            stopEventFetcher)

  private def stopEventFetcher: IO[Unit] =
    logger.traceIO:
      eventFetcher.terminateAndLogout *>
        untilFetchingStopped.get

  private val eventFetcher = new RecouplingStreamReader[EventId, Stamped[AnyKeyedEvent], AgentClient](
      _.eventId.some, conf.recouplingStreamReader):

    private var attachedOrderIds: Set[OrderId] | Null = null

    override protected def couple(eventId: EventId) =
      journal.aggregate
        .map(_.keyTo(AgentRefState).checked(agentPath))
        .flatMapT: agentRefState =>
          (agentRefState.couplingState, agentRefState.agentRunId) match
            case (Resetting(false), None) =>
              IO.pure(Left(Problem.pure("Resetting, but no AgentRunId?"))) // Invalid state

            case (Resetting(force), maybeAgentRunId) if force || maybeAgentRunId.isDefined =>
              resetAgent(maybeAgentRunId)
                .flatMapT: _ =>
                  journal.persist:
                    _.keyTo(AgentRefState).checked(agentPath).map: agentRefState =>
                      agentRefState.couplingState match
                        case _: Resetting => Some(agentPath <-: AgentReset)
                        case _ => None
            case _ =>
              IO.pure(Checked.unit)
        .flatMapT: _ =>
          dedicateAgentIfNeeded(directorDriver)
        .flatMapT: (agentRunId, agentEventId) =>
          val coupleController = CoupleController(agentPath, agentRunId, eventId = agentEventId,
            ControllerRunId(journal.journalId))
          executeCommand(coupleController).flatMapT:
            case CoupleController.Response(orderIds) =>
              logger.trace:
                s"CoupleController returned attached OrderIds={${orderIds.toSeq.sorted.mkString(" ")}}"
              attachedOrderIds = orderIds
              journal.persist: controllerState =>
                controllerState.keyTo(AgentRefState).checked(agentPath).map: a =>
                  Seq(
                    (a.couplingState != Coupled || a.problem.nonEmpty) ?
                      (agentPath <-: AgentCoupled),
                    // The coupled Agent may not yet have an AgentRef (containing the
                    // processLimit). So we need to check this:
                    !controllerState.itemToAgentToAttachedState.contains(agentPath) ?
                      (NoKey <-: ItemAttachable(agentPath, agentPath))
                  ).flatten
              .flatTapT: persisted =>
                IO.whenA(persisted.keyedEvents.exists(_.event.isInstanceOf[ItemAttachable])):
                  persisted.aggregate.keyToItem(AgentRef).get(agentPath).fold(IO.unit): agentRef =>
                    agentDriver.send(AgentDriver.Queueable.AttachUnsignedItem(agentRef))
                .as(Checked.unit)
              .rightAs(agentEventId)

    protected def getStream(api: AgentClient, after: EventId) =
      IO(logger.debug(s"getStream(after=$after)")) *>
        api
          // A Pekko HTTP stream seems not to be cancelable with interruptWhen.
          // So we use a fast heartbeat and stop at the next stream element.
          .agentEventStream(
            EventRequest[Event](
              EventClasses,
              after = after,
              timeout = None /*no timeout due to heartbeat keep-alive (otherwise requestTimeout)*/),
            heartbeat = conf.recouplingStreamReader.keepAlive.some,
            idleTimeout = idleTimeout,
            dontLog = true)
          .map(_.map(_
            .recoverWith(PekkoHttpClient.warnIdleTimeout)
            .interruptWhenF(untilStopRequested)))
          .race(untilStopRequested)
          .flatMap:
            case Left(checkedStream) => IO.pure(checkedStream)
            case Right(()) => IO.right(Stream.empty)

    override protected def onCouplingFailed(api: AgentClient, problem: Problem) =
      onCouplingFailed_(problem)

    override protected def onCoupled(api: AgentClient, after: EventId) =
      IO.defer:
        logger.info(s"Coupled with $api after=${EventId.toString(after)}")
        onCoupled_(attachedOrderIds.nn)
          .*>(IO:
            attachedOrderIds = null)
          .as(Completed)

    override protected def onDecoupled =
      IO.defer:
        logger.debug("onDecoupled")
        onDecoupled_
          .as(Completed)

    protected def stopRequested = directorDriver.isStopping

  private def continuallyFetchEvents: IO[Unit] =
   logger.traceIO:
    observeAndConsumeEvents
      .handleError(t => logger.error(t.toStringWithCauses, t))
      .flatMapLoop(()): (_, _, again) =>
        eventFetcher.decouple
          .*>(eventFetcher
            .pauseBeforeNextTry(conf.recouplingStreamReader.delay)
            .raceMerge(untilStopRequested))
          .void
          .handleError(t => logger.error(t.toStringWithCauses, t))
          .*>(IO.defer(IO.unlessA(isStopping):
            again(())))
      .guarantee:
        untilFetchingStopped.complete(()).void

  private def observeAndConsumeEvents: IO[Unit] =
    logger.traceIO(IO.defer:
      val delay = conf.eventBufferDelay max conf.commitDelay
      eventFetcher.stream(client, after = adoptedEventId)
        .pipe(stream =>
          if delay.isZeroOrBelow then
            stream.chunks
          else
            stream.groupWithin(chunkSize = conf.eventBufferSize, delay))
        .interruptWhenF(untilStopRequested)
        .evalMapChunk: chunk =>
          // When the other cluster node may have failed-over,
          // wait until we know that it hasn't (or this node is aborted).
          // Avoids "Unknown OrderId" failures due to double activation.
          journal.whenNoFailoverByOtherNode
            .logWhenItTakesLonger("whenNoFailoverByOtherNode")
            .as(chunk)
        .map(_.asSeq)
        .evalMap(onEventsFetched)
        .completedL)

  private def onEventsFetched(stampedEvents: Seq[Stamped[AnyKeyedEvent]]): IO[Unit] =
    onFetchedEventsLock.lock(logger.traceIO(IO.defer:
      assertThat(stampedEvents.nonEmpty)
      if isStopping then
        IO(logger.debug:
          s"❌Late onEventsFetched(${stampedEvents.size} events) suppressed due to isStopping")
      else
        val reducedStampedEvents = stampedEvents.dropWhile: stamped =>
          val drop = stamped.eventId <= adoptedEventId
          if drop then logger.debug(s"Drop duplicate received event: $stamped")
          drop
        IO.whenA(reducedStampedEvents.nonEmpty):
          val lastEventId = stampedEvents.last.eventId
          // The events must be journaled and handled by ControllerOrderKeeper
          adoptedEventId = lastEventId
          adoptEvents(reducedStampedEvents)
    ))

  def resetAgentAndStop(agentRunId: Option[AgentRunId]): IO[Checked[Unit]] =
    // TODO If stopEventFetcher would not send a Logout, it could run concurrently
    resetAgent(agentRunId)
      .recoverFromProblem:
        case problem @ AgentNotDedicatedProblem =>
          logger.debug(s"resetAgent: $problem")
      .flatTapT(_ => stop.map(Right(_)))

  private def resetAgent(agentRunId: Option[AgentRunId]): IO[Checked[Unit]] =
    logger.debugIO:
      executeCommand(client, AgentCommand.Reset(agentRunId))
        .logWhenMethodTakesLonger
        .recoverFromProblem:
          case problem @ AgentNotDedicatedProblem =>
            logger.info(s"AgentCommand.Reset: Agent is already reset: $problem")
        .rightAs(())

  def executeCommand(command: AgentCommand, mustBeCoupled: Boolean = false)
  : IO[Checked[command.Response]] =
    if mustBeCoupled then
      eventFetcher.coupledApi
        .map(_.toRight(DecoupledProblem))
        .flatMapT(executeCommand(_, command))
    else
      executeCommand(client, command)

  private def executeCommand(client: AgentClient, command: AgentCommand)
  : IO[Checked[command.Response]] =
    IO
      .race(
        untilStopRequested.delayBy(10.s/*because AgentDriver stops on SwitchOver*/),
        client.retryIfSessionLost:
          client.commandExecute(command))
      .map:
        case Left(()) => Left(DirectorDriverStoppedProblem(agentPath))
        case Right(o) => o

  override def toString = s"DirectorDriver($agentPath #$index)"


private[agent] object DirectorDriver:
  private val index = Atomic(0)

  private val EventClasses = Set[Class[? <: Event]](
    classOf[OrderEvent],
    classOf[AgentEvent.AgentReady],
    AgentEvent.AgentShutDown.getClass,
    classOf[SubagentItemStateEvent],
    classOf[InventoryItemEvent],
    classOf[OrderWatchEvent],
    classOf[ClusterEvent])

  def resource(
    agentDriver: AgentDriver,
    agentPath: AgentPath,
    initialEventId: EventId,
    client: AgentClient,
    dedicateAgentIfNeeded: DirectorDriver => IO[Checked[(AgentRunId, EventId)]],
    onCouplingFailed: Problem => IO[Boolean],
    onCoupled: Set[OrderId] => IO[Unit],
    onDecoupled: IO[Unit],
    adoptEvents: Seq[Stamped[AnyKeyedEvent]] => IO[Unit],  // TODO Stream
    journal: Journal[ControllerState],
    conf: AgentDriverConfiguration)
  : ResourceIO[DirectorDriver] =
    Service.resource:
      DirectorDriver(
        agentDriver, agentPath, initialEventId, client,
        dedicateAgentIfNeeded,
        onCouplingFailed, onCoupled, onDecoupled, adoptEvents,
        journal, conf)

  final case class DirectorDriverStoppedProblem(agentPath: AgentPath)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "agentPath" -> agentPath.string)

  object DirectorDriverStoppedProblem extends Problem.Coded.Companion
