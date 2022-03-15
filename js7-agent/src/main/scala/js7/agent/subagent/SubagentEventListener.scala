package js7.agent.subagent

import cats.effect.ExitCase
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.traverse._
import js7.agent.subagent.SubagentEventListener._
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.log.Logger.syntax._
import js7.base.monixutils.MonixBase.syntax.{RichMonixObservable, RichMonixTask}
import js7.base.monixutils.ObservablePauseDetector.RichPauseObservable
import js7.base.monixutils.Switch
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
import js7.data.subagent.SubagentRefStateEvent.{SubagentCoupled, SubagentCouplingFailed, SubagentEventsObserved, SubagentShutdown}
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
  private lazy val stdoutCommitOptions = CommitOptions(delay = subagentConf.stdoutCommitDelay)  // TODO Use it!
  private val stopObserving = MVar.empty[Task, Unit]().memoize
  private val observingStopped = MVar.empty[Task, Unit]().memoize
  private val _isHeartbeating = Atomic(false)
  private val isListening = Atomic(false)

  protected final val coupled = Switch(false)

  protected final def stopEventListener: Task[Unit] =
    logger.debugTask("stopEventListener")(Task.defer(
      Task.when(isListening.getAndSet(false))(
        stopObserving.flatMap(_.tryPut(())) *>
          observingStopped.flatMap(_.tryRead).void)))

  protected final def startEventListener: Task[Unit] =
    logger.debugTask(Task.defer {
      if (isListening.getAndSet(true)) {
        val msg = "Duplicate startEventListener"
        logger.error(msg)
        Task.raiseError(new RuntimeException(s"$toString: $msg"))
      } else
        logger.debugTask("Observing events")(
          observeEvents
        ).startAndForget
    })

  private def observeEvents: Task[Unit] = {
    val recouplingStreamReader = newEventListener()
    recouplingStreamReader
      .observe(client, after = persistence.currentState.idToSubagentRefState(subagentId).eventId)
      .takeUntilEval(stopObserving.flatMap(_.read))
      .buffer(
        Some(conf.eventBufferDelay max conf.commitDelay),
        maxCount = conf.eventBufferSize)  // ticks
      .filter(_.nonEmpty)   // Ignore empty ticks
      .mapEval { stampedSeq =>
        val lastEventId = stampedSeq.last.eventId
        val (updatedStampedSeq0, followUps) = stampedSeq.map(handleEvent).unzip
        val updatedStampedSeq = updatedStampedSeq0.flatten
        val events = updatedStampedSeq.view.map(_.value)
          .concat((subagentId <-: SubagentEventsObserved(lastEventId)) :: Nil)
          .toVector
        // TODO Save Stamped timestamp
        persistence
          .persistKeyedEvents(events, CommitOptions(transaction = true))
          .map(_.orThrow/*???*/)
          // After an OrderProcessed event the observed EventIds must be released at the Subagent,
          // to terminate StartOrderProcess command idempotency detection and
          // allow a new StartOrderProcess command for a next process.
          .*>(updatedStampedSeq
            .collect { case Stamped(_, _, KeyedEvent(o: OrderId, _: OrderProcessed)) => o }
            .traverse(detachProcessedOrder))
          .*>(releaseEvents(lastEventId))
          .*>(followUps.combineAll)
      }
      .guarantee(recouplingStreamReader
        .terminateAndLogout
        .void
        .logWhenItTakesLonger)
      .completedL
  }

  private def handleEvent(stamped: Stamped[AnyKeyedEvent])
  : (Option[Stamped[AnyKeyedEvent]], Task[Unit]) =
    stamped.value match {
      case keyedEvent @ KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        event match {
          // TODO Discard (while logging an error) inapplicable (wrong) events
          case _: OrderStdWritten =>
            // TODO Save Timestamp
            Some(stamped) -> Task.unit

          case orderProcessed: OrderProcessed =>
            // TODO Save Timestamp
            Some(stamped) -> onOrderProcessed(orderId, orderProcessed)

          case _ =>
            logger.error(s"Unexpected event: $keyedEvent")
            None -> Task.unit
        }

      case KeyedEvent(_: NoKey, SubagentEvent.SubagentShutdown) =>
        None -> onSubagentDied(SubagentShutdown)

      case KeyedEvent(_: NoKey, event: SubagentEvent) =>
        logger.debug(event.toString)
        None -> Task.unit

      case keyedEvent =>
        logger.error(s"Unexpected event: $keyedEvent")
        None -> Task.unit
    }

  private def newEventListener() =
    new RecouplingStreamReader[EventId, Stamped[AnyKeyedEvent], SubagentClient](
      _.eventId, recouplingStreamReaderConf)
    {
      private var lastProblem: Option[Problem] = None
      override protected def idleTimeout = None  // SubagentEventListener itself detects heartbeat loss

      override protected def couple(eventId: EventId) =
        logger.traceTask(
          dedicateOrCouple
            .flatMapT { case (_, eventId) =>
              coupled.switchOn
                .as(Right(eventId))
            })

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
                .flatTap(stamped =>
                  if (stamped ne PauseStamped)
                    onHeartbeatStarted
                  else {
                    val problem = Problem.pure(s"Missing heartbeat from $subagentId")
                    logger.warn(problem.toString)
                    Observable.fromTask(onSubagentDecoupled(Some(problem)))
                  })
                .filter(_ != StampedHeartbeat)
                .takeWhile(_ != PauseStamped)
                .guaranteeCase(exitCase => Task.defer(
                  Task.when(exitCase != ExitCase.Completed || isHeartbeating)(
                    Task.defer {
                      val problem = Problem.pure(s"$subagentId event stream: $exitCase")
                      logger.warn(problem.toString)
                      onSubagentDecoupled(Some(problem))
                    }))))
              .map(Right(_))
        }

      override protected def onCouplingFailed(api: SubagentClient, problem: Problem) =
        onSubagentDecoupled(Some(problem)) *>
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
        onSubagentDecoupled(None) *>
          Task.defer {
            logger.debug("onDecoupled")
            coupled.switchOff
              .as(Completed)
          }

      protected def stopRequested = false
    }

  private def onHeartbeatStarted: Observable[Unit] =
    Observable.fromTask(
      Task.defer(Task.when(!_isHeartbeating.getAndSet(true))(
        // Different to AgentDriver,
        // for Subagents the Coupling state is tied to the continuous flow of events.
        persistence.persistKeyedEvent(subagentId <-: SubagentCoupled)
          .map(_.orThrow))))

  private def onSubagentDecoupled(problem: Option[Problem]): Task[Unit] =
    logger.traceTask("onSubagentDecoupled", problem.toString)(
      Task.defer(Task.when(_isHeartbeating.getAndSet(false))(
        persistence
          .lock(subagentId)(
            persistence.persist(_
              .idToSubagentRefState.checked(subagentId)
              .map { subagentRefState =>
                val prblm = problem
                  .orElse(subagentRefState.problem)
                  .getOrElse(Problem.pure("decoupled"))
                (!subagentRefState.problem.contains(prblm))
                  .thenList(subagentId <-: SubagentCouplingFailed(prblm))
              }))
          .map(_.orThrow))))

  final def isHeartbeating = _isHeartbeating.get()
}

object SubagentEventListener
{
  private val PauseStamped: Stamped[KeyedEvent[Event]] = Stamped(0L, null)
  private[subagent] val heartbeatTiming = HeartbeatTiming(3.s, 10.s)  // TODO
}
