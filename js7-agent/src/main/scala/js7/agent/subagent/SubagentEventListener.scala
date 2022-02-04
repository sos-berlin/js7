package js7.agent.subagent

import cats.effect.ExitCase
import cats.syntax.flatMap._
import js7.agent.subagent.SubagentEventListener._
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.log.Logger.syntax._
import js7.base.monixutils.MonixBase.syntax.{RichMonixObservable, RichMonixTask}
import js7.base.monixutils.ObservablePauseDetector.RichPauseObservable
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.common.http.RecouplingStreamReader
import js7.data.event.JournalEvent.StampedHeartbeat
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, KeyedEvent, Stamped}
import js7.data.order.OrderEvent.{OrderProcessed, OrderStdWritten}
import js7.data.order.{OrderEvent, OrderId}
import js7.data.other.HeartbeatTiming
import js7.data.subagent.SubagentRefStateEvent.{SubagentCoupled, SubagentEventsObserved, SubagentLost, SubagentShutdown}
import js7.journal.CommitOptions
import js7.subagent.SubagentState.keyedEventJsonCodec
import js7.subagent.client.SubagentClient
import js7.subagent.data.SubagentEvent
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.atomic.Atomic
import monix.reactive.Observable

trait SubagentEventListener
{
  this: RemoteSubagentDriver =>

  private val logger = Logger.withPrefix[SubagentEventListener](subagentId.toString)
  private lazy val stdoutCommitOptions = CommitOptions(delay = conf.stdoutCommitDelay)
  private val stopObserving = MVar.empty[Task, Unit]().memoize
  private val observingStopped = MVar.empty[Task, Unit]().memoize
  private val _isHeartbeating = Atomic(false)

  protected final def stopEventListener: Task[Unit] =
    logger.debugTask("stopEventListener")(
      stopObserving.flatMap(_.tryPut(())) *>
        observingStopped.flatMap(_.tryRead).void)


  protected final def startEventListener: Task[Unit] =
    Task.defer {
      logger.debug("startEventListener")
      val recouplingStreamReader = newEventListener()
      recouplingStreamReader
        .observe(client, after = persistence.currentState.idToSubagentRefState(subagentId).eventId)
        .takeUntilEval(stopObserving.flatMap(_.read))
        .mapEval { stamped =>
          val observed = subagentId <-: SubagentEventsObserved(stamped.eventId)
          stamped.value match {
            case keyedEvent @ KeyedEvent(orderId: OrderId, event: OrderEvent) =>
              event match {
                case _: OrderStdWritten =>
                  // TODO Save Timestamp
                  persistence
                    .persistKeyedEvents(keyedEvent :: observed :: Nil/*, stdoutCommitOptions,
                      commitLater = conf.stdoutCommitDelay.isPositive*/)
                    .map(_.orThrow/*???*/)

                case orderProcessed: OrderProcessed =>
                  // TODO Save Timestamp
                  persistence
                    .persistKeyedEvents(keyedEvent :: observed :: Nil)
                    .map(_.orThrow/*???*/)
                    .*>(onOrderProcessed(orderId, orderProcessed))

                case _ =>
                  logger.error(s"Unexpected event: $keyedEvent")
                  Task.unit
              }

            case KeyedEvent(_: NoKey, SubagentEvent.SubagentShutdown) =>
              onSubagentDied(SubagentShutdown)

            case KeyedEvent(_: NoKey, event: SubagentEvent) =>
              logger.debug(event.toString)
              persistence.persistKeyedEvent(observed)
                .map(_.orThrow)

            case keyedEvent =>
              logger.error(s"Unexpected event: $keyedEvent")
              Task.unit
          }
        }
        .guarantee(
          recouplingStreamReader.terminateAndLogout.void
            .logWhenItTakesLonger)
        .guaranteeCase(exitCase => Task {
          logger.debug(s"Observing events => $exitCase")
        })
        .completedL
        .startAndForget
    }

  private def newEventListener() =
    new RecouplingStreamReader[EventId, Stamped[AnyKeyedEvent], SubagentClient](
      _.eventId, recouplingStreamReaderConf)
    {
      private var lastProblem: Option[Problem] = None
      override protected def idleTimeout = heartbeatTiming.longHeartbeatTimeout + 2.s

      override protected def couple(eventId: EventId) =
        dedicateOrCouple

      protected def getObservable(api: SubagentClient, after: EventId) =
        Task.defer {
          logger.debug(s"getObservable(after=$after)")
          api.login(onlyIfNotLoggedIn = true) *>
            api
              .eventObservable(
                EventRequest.singleClass[Event](after = after, timeout = None),
                heartbeat = Some(heartbeatTiming.heartbeat))
              .map(_
                .detectPauses2(heartbeatTiming.longHeartbeatTimeout, PauseStamped)
                .tapEach(o => logger.debug("### " + o.toString.truncateWithEllipsis(200)))
                .flatTap(stamped =>
                  if (stamped ne PauseStamped)
                    onHeartbeatStarted
                  else {
                    val problem = Problem.pure(s"Missing heartbeat from $subagentId")
                    logger.warn(problem.toString)
                    Observable.fromTask(onHeartbeatLost(problem))
                  })
                .filter(_ != StampedHeartbeat)
                .takeWhile(_ != PauseStamped)
                .guaranteeCase(exitCase => Task(
                  Task.when(exitCase != ExitCase.Completed || isHeartbeating)(
                    onHeartbeatLost(Problem.pure(s"$subagentId event stream: $exitCase"))))))
              .map(Right(_))
        }

      override protected def onCouplingFailed(api: SubagentClient, problem: Problem) =
        onHeartbeatLost(problem) *>
          Task {
            if (lastProblem contains problem) {
              logger.debug(s"Coupling failed: $problem")
            } else {
              lastProblem = Some(problem)
              logger.warn(s"Coupling failed: $problem")
            }
            true
          }

      override protected def onDecoupled =
        onHeartbeatLost(Problem.pure(s"$subagentId decoupled")) *>
          Task {
            logger.debug("onDecoupled")
            Completed
          }

      protected def stopRequested = false
    }

  private def onHeartbeatStarted: Observable[Unit] =
    Observable.fromTask(Task.defer {
      Task.when(!_isHeartbeating.getAndSet(true))(
        // Different to AgentDriver,
        // for Subagents the Coupling state is tied to the continuous flow of events.
        persistence.persistKeyedEvent(subagentId <-: SubagentCoupled)
          .map(_.orThrow))
    })

  private def onHeartbeatLost(problem: Problem): Task[Unit] =
    Task.defer {
      logger.warn(problem.toString)
      Task.when(_isHeartbeating.getAndSet(false))(
        persistence
          .lock(subagentId)(
            persistence.persist(_
              .idToSubagentRefState.checked(subagentId)
              .map(subagentRefState =>
                (!subagentRefState.problem.contains(problem))
                  .thenList(subagentId <-: SubagentLost(problem)))))
          .map(_.orThrow))
    }

  final def isHeartbeating = _isHeartbeating.get()
}

object SubagentEventListener
{
  private val PauseStamped: Stamped[KeyedEvent[Event]] = Stamped(0L, null)
  private[subagent] val heartbeatTiming = HeartbeatTiming(3.s, 10.s)  // TODO
}
