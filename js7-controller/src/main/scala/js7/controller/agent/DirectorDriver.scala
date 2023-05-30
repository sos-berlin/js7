package js7.controller.agent

import cats.effect.Resource
import cats.effect.concurrent.Deferred
import js7.agent.client.AgentClient
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.CoupleController
import js7.agent.data.event.AgentEvent
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.http.RecouplingStreamReader
import js7.controller.agent.AgentDriver.DecoupledProblem
import js7.controller.agent.DirectorDriver.*
import js7.data.agent.AgentRefStateEvent.{AgentCoupled, AgentReset}
import js7.data.agent.Problems.AgentNotDedicatedProblem
import js7.data.agent.{AgentPath, AgentRefState, AgentRunId}
import js7.data.cluster.ClusterEvent
import js7.data.controller.ControllerState
import js7.data.delegate.DelegateCouplingState.{Coupled, Resetting}
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, Stamped}
import js7.data.item.InventoryItemEvent
import js7.data.order.{OrderEvent, OrderId}
import js7.data.orderwatch.OrderWatchEvent
import js7.data.subagent.SubagentItemStateEvent
import js7.journal.state.Journal
import monix.eval.Task
import monix.reactive.Observable
import scala.util.chaining.scalaUtilChainingOps

private[agent] final class DirectorDriver private(
  agentPath: AgentPath,
  initialEventId: EventId,
  client: AgentClient,
  dedicateAgentIfNeeded: DirectorDriver => Task[Checked[(AgentRunId, EventId)]],
  onCouplingFailed_ : Problem => Task[Boolean],
  onCoupled_ : Set[OrderId] => Task[Unit],
  onDecoupled_ : Task[Unit],
  adoptEvents: Seq[Stamped[AnyKeyedEvent]] => Task[Unit],
  journal: Journal[ControllerState],
  conf: AgentDriverConfiguration)
extends Service.StoppableByRequest
{
  directorDriver =>

  private val logger = Logger.withPrefix[this.type](agentPath.toString)
  private var adoptedEventId = initialEventId
  private val untilFetchingStopped = Deferred.unsafe[Task, Unit]

  logger.trace(s"initialEventId=$initialEventId")

  protected def start =
    startService(
      continuallyFetchEvents *>
        Task(assertThat(isStopping)) *>
        onFetchedEventsLock.lock(Task.unit) *>
        stopEventFetcher)

  private def stopEventFetcher: Task[Unit] =
    eventFetcher.terminateAndLogout *>
      untilFetchingStopped.get

  private val eventFetcher = new RecouplingStreamReader[EventId, Stamped[AnyKeyedEvent], AgentClient](
      _.eventId, conf.recouplingStreamReader)
  {
    private var attachedOrderIds: Set[OrderId] = null

    override protected def couple(eventId: EventId) =
      journal.state
        .map(_.keyTo(AgentRefState).checked(agentPath))
        .flatMapT(agentRefState =>
          ((agentRefState.couplingState, agentRefState.agentRunId) match {
            case (Resetting(false), None) =>
              Task.pure(Left(Problem.pure("Resetting, but no AgentRunId?"))) // Invalid state

            case (Resetting(force), maybeAgentRunId) if force || maybeAgentRunId.isDefined =>
              executeCommand(AgentCommand.Reset(maybeAgentRunId))
                .map {
                  case Left(AgentNotDedicatedProblem) => Checked.unit // Already reset
                  case o => o
                }
                .flatMapT(_ =>
                  journal.persistKeyedEvent(agentPath <-: AgentReset))

            case _ =>
              Task.pure(Checked.unit)
          }))
        .flatMapT(_ => dedicateAgentIfNeeded(directorDriver))
        .flatMapT { case (agentRunId, agentEventId) =>
          executeCommand(CoupleController(agentPath, agentRunId, eventId = agentEventId))
            .flatMapT { case CoupleController.Response(orderIds) =>
              logger.trace(s"CoupleController returned attached OrderIds={${orderIds.toSeq.sorted.mkString(" ")}}")
              attachedOrderIds = orderIds
              journal
                .lock(agentPath)(
                  journal.persist(controllerState =>
                    for (a <- controllerState.keyTo(AgentRefState).checked(agentPath)) yield
                      (a.couplingState != Coupled || a.problem.nonEmpty)
                        .thenList(agentPath <-: AgentCoupled)))
                .rightAs(agentEventId)
            }
        }

    protected def getObservable(api: AgentClient, after: EventId) =
      Task {logger.debug(s"getObservable(after=$after)")} *>
        api.eventObservable(EventRequest[Event](EventClasses, after = after,
          timeout = Some(requestTimeout)))

    override protected def onCouplingFailed(api: AgentClient, problem: Problem) =
      onCouplingFailed_(problem)

    override protected def onCoupled(api: AgentClient, after: EventId) =
      Task.defer {
        logger.info(s"Coupled with $api after=${EventId.toString(after)}")
        assertThat(attachedOrderIds != null)
        onCoupled_(attachedOrderIds)
          .*>(Task {
            attachedOrderIds = null
          })
          .as(Completed)
      }

    override protected def onDecoupled =
      Task.defer {
        logger.debug("onDecoupled")
        onDecoupled_
          .as(Completed)
      }

    protected def stopRequested = directorDriver.isStopping
  }

  private def continuallyFetchEvents: Task[Unit] =
    observeAndConsumeEvents
      .onErrorHandle(t => logger.error(t.toStringWithCauses, t))
      .flatMapLoop(())((_, _, again) =>
        eventFetcher.decouple
          .*>(eventFetcher
            .pauseBeforeNextTry(conf.recouplingStreamReader.delay)
            .raceFold(untilStopRequested))
          .materialize
          .onErrorHandle(t => logger.error(t.toStringWithCauses, t))
          .*>(Task.defer(Task.unless(isStopping)(
            again(())))))
      //.raceFold(untilStopRequested)
      .guarantee(untilFetchingStopped.complete(()))

  private def observeAndConsumeEvents: Task[Completed] =
    logger.traceTask(Task.defer {
      val delay = conf.eventBufferDelay max conf.commitDelay
      eventFetcher.observe(client, after = adoptedEventId)
        .pipe(obs =>
          if (delay.isZeroOrBelow)
            obs.bufferIntrospective(conf.eventBufferSize)
          else obs
            .buffer(
              Some(conf.eventBufferDelay max conf.commitDelay),
              maxCount = conf.eventBufferSize) // ticks
            .filter(_.nonEmpty)) // Ignore empty ticks
        .flatMap(o => Observable
          // When the other cluster node may have failed-over,
          // wait until we know that it hasn't (or this node is aborted).
          // Avoids "Unknown OrderId" failures due to double activation.
          .fromTask(journal.whenNoFailoverByOtherNode
            .logWhenItTakesLonger("whenNoFailoverByOtherNode"))
          .map(_ => o))
        .mapEval(onEventsFetched)
        .takeUntilEval(untilStopRequested)
        .completedL
        .as(Completed)
    })

  private def onEventsFetched(stampedEvents: Seq[Stamped[AnyKeyedEvent]]): Task[Unit] =
    Task.defer {
      assertThat(stampedEvents.nonEmpty)
      val reducedStampedEvents = stampedEvents dropWhile { stamped =>
        val drop = stamped.eventId <= adoptedEventId
        if (drop) logger.debug(s"Drop duplicate received event: $stamped")
        drop
      }
      Task.when(reducedStampedEvents.nonEmpty) {
        val lastEventId = stampedEvents.last.eventId
        // The events must be journaled and handled by ControllerOrderKeeper
        adoptedEventId = lastEventId
        adoptEvents(reducedStampedEvents)
      }
    }

  def resetAgentAndStop(agentRunId: Option[AgentRunId]): Task[Checked[Unit]] =
    // TODO If stopEventFetcher would not send a Logout, it could run concurrently
    resetAgent(agentRunId)
      .flatTapT(_ => stop.map(Right(_)))

  private def resetAgent(agentRunId: Option[AgentRunId]): Task[Checked[Unit]] =
    logger.debugTask(
      executeCommand(client, AgentCommand.Reset(agentRunId))
        .logWhenItTakesLonger
        .rightAs(()))

  def executeCommand(command: AgentCommand, mustBeCoupled: Boolean = false)
  : Task[Checked[command.Response]] =
    if (mustBeCoupled)
      eventFetcher.coupledApi
        .map(_.toRight(DecoupledProblem))
        .flatMapT(executeCommand(_, command))
    else
      executeCommand(client, command)

  private def executeCommand(client: AgentClient, command: AgentCommand)
  : Task[Checked[command.Response]] =
    Task
      .race(
        untilStopRequested,
        client.retryIfSessionLost()(
          client.commandExecute(command)))
      .map {
        case Left(()) => Left(DirectorDriverStoppedProblem(agentPath))
        case Right(o) => o
      }

  override def toString = s"DirectorDriver($agentPath)"
}

private[agent] object DirectorDriver {
  private val EventClasses = Set[Class[? <: Event]](
    classOf[OrderEvent],
    classOf[AgentEvent.AgentReady],
    AgentEvent.AgentShutDown.getClass,
    classOf[SubagentItemStateEvent],
    classOf[InventoryItemEvent],
    classOf[OrderWatchEvent],
    classOf[ClusterEvent])

  def resource(
    agentPath: AgentPath,
    initialEventId: EventId,
    client: AgentClient,
    dedicateAgentIfNeeded: DirectorDriver => Task[Checked[(AgentRunId, EventId)]],
    onCouplingFailed: Problem => Task[Boolean],
    onCoupled: Set[OrderId] => Task[Unit],
    onDecoupled: Task[Unit],
    adoptEvents: Seq[Stamped[AnyKeyedEvent]] => Task[Unit],  // TODO Stream
    journal: Journal[ControllerState],
    conf: AgentDriverConfiguration)
  : Resource[Task, DirectorDriver] =
    Service.resource(Task(
      new DirectorDriver(
        agentPath, initialEventId, client,
        dedicateAgentIfNeeded,
        onCouplingFailed, onCoupled, onDecoupled, adoptEvents,
        journal, conf)))

  final case class DirectorDriverStoppedProblem(agentPath: AgentPath)
  extends Problem.Coded {
    def arguments = Map("agentPath" -> agentPath.string)
  }
}
