package js7.subagent.director

import cats.effect.ExitCase
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.traverse._
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.log.Logger.syntax._
import js7.base.monixutils.MonixBase.syntax.{RichMonixObservable, RichMonixTask}
import js7.base.monixutils.ObservablePauseDetector.RichPauseObservable
import js7.base.monixutils.Switch
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax._
import js7.common.http.RecouplingStreamReader
import js7.data.event.JournalEvent.StampedHeartbeat
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, KeyedEvent, Stamped}
import js7.data.order.OrderEvent.{OrderProcessed, OrderStdWritten}
import js7.data.order.{OrderEvent, OrderId}
import js7.data.other.HeartbeatTiming
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentEventsObserved, SubagentShutdown}
import js7.data.subagent.SubagentState.keyedEventJsonCodec
import js7.data.subagent.{SubagentDirectorState, SubagentEvent, SubagentRunId}
import js7.journal.CommitOptions
import js7.subagent.director.SubagentEventListener._
import monix.catnap.MVar
import monix.eval.{Fiber, Task}
import monix.execution.atomic.Atomic
import monix.reactive.Observable
import scala.util.chaining.scalaUtilChainingOps

private trait SubagentEventListener[S <: SubagentDirectorState[S]]
{
  this: RemoteSubagentDriver[S] =>

  private val logger = Logger.withPrefix[SubagentEventListener[S]](subagentItem.pathRev.toString)
  private lazy val stdoutCommitOptions = CommitOptions(delay = subagentConf.stdoutCommitDelay)  // TODO Use it!
  private val stopObserving = MVar.empty[Task, Unit]().memoize
  @volatile private var observing: Fiber[Unit] = Fiber(Task.unit, Task.unit)
  private val _isHeartbeating = Atomic(false)
  private val isListening = Atomic(false)
  private val lock = AsyncLock()

  protected final val coupled = Switch(false)

  protected final def stopEventListener: Task[Unit] =
    lock.lock(
      logger.debugTask(Task.defer(
        Task.when(isListening.getAndSet(false))(Task.defer {
          stopObserving
            .flatMap(_.tryPut(())).void
            .*>(Task.defer(observing.join))
        }))))

  protected final def startEventListener: Task[Unit] =
    lock.lock(
      logger.debugTask(Task.defer {
        if (isListening.getAndSet(true)) {
          val msg = "Duplicate startEventListener"
          logger.error(msg)
          Task.raiseError(new RuntimeException(s"$toString: $msg"))
        } else
          stopObserving.flatMap(_.tryTake)
            .*>(observeEvents)
            .start
            .flatMap(fiber => Task {
              observing = fiber
            })
      }))

  private def observeEvents: Task[Unit] = {
    val recouplingStreamReader = newEventListener()
    val bufferDelay = conf.eventBufferDelay max conf.commitDelay
    logger.debugTask(recouplingStreamReader
      .observe(client, after = persistence.currentState.idToSubagentItemState(subagentId).eventId)
      .takeUntilEval(stopObserving.flatMap(_.read))
      .pipe(obs =>
        if (!bufferDelay.isPositive)
          obs.map(_ :: Nil)
        else
          obs.buffer(
            Some(conf.eventBufferDelay max conf.commitDelay),
            maxCount = conf.eventBufferSize)  // ticks
          .filter(_.nonEmpty))   // Ignore empty ticks
      .mapEval(_
        .traverse(handleEvent)
        .flatMap { updatedStampedSeq0 =>
          val (updatedStampedSeqSeq, followUps) = updatedStampedSeq0.unzip
          val updatedStampedSeq = updatedStampedSeqSeq.flatten
          val lastEventId = updatedStampedSeq.lastOption.map(_.eventId)
          val events = updatedStampedSeq.view.map(_.value)
            .concat(lastEventId.map(subagentId <-: SubagentEventsObserved(_)))
            .toVector
          // TODO Save Stamped timestamp
          persistence
            .persistKeyedEvents(events, CommitOptions(transaction = true))
            .map(_.orThrow/*???*/)
            // After an OrderProcessed event an DetachProcessedOrder must be sent,
            // to terminate StartOrderProcess command idempotency detection and
            // allow a new StartOrderProcess command for a next process.
            .*>(updatedStampedSeq
              .collect { case Stamped(_, _, KeyedEvent(o: OrderId, _: OrderProcessed)) => o }
              .traverse(detachProcessedOrder))
            .*>(lastEventId.traverse(releaseEvents))
            .*>(followUps.combineAll)
        })
      .guarantee(recouplingStreamReader
        .terminateAndLogout
        .void
        .logWhenItTakesLonger)
      .completedL)
  }

  /** Returns optionally the event and a follow-up task. */
  private def handleEvent(stamped: Stamped[AnyKeyedEvent])
  : Task[(Option[Stamped[AnyKeyedEvent]], Task[Unit])] =
    stamped.value match {
      case keyedEvent @ KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        event match {
          case _: OrderStdWritten =>
            // TODO Save Timestamp
            Task.pure(Some(stamped) -> Task.unit)

          case orderProcessed: OrderProcessed =>
            // TODO Save Timestamp
            onOrderProcessed(orderId, orderProcessed).map {
              case None => None -> Task.unit  // OrderProcessed already handled
              case Some(followUp) =>
                // The followUp Task notifies OrderActor about OrderProcessed by calling `onEvents`
                Some(stamped) -> followUp
            }

          case _ =>
            logger.error(s"Unexpected event: $keyedEvent")
            Task.pure(None -> Task.unit)
        }

      case KeyedEvent(_: NoKey, SubagentEvent.SubagentShutdown) =>
        Task.pure(None -> onSubagentDied(SubagentShutdown))

      case KeyedEvent(_: NoKey, event: SubagentEvent.SubagentItemAttached) =>
        // TODO Subagent should not emit unused events
        logger.debug(event.toString)
        Task.pure(None -> Task.unit)

      case keyedEvent =>
        logger.error(s"Unexpected event: $keyedEvent")
        Task.pure(None -> Task.unit)
    }

  private def newEventListener() =
    new RecouplingStreamReader[EventId, Stamped[AnyKeyedEvent], SubagentClient](
      _.eventId, recouplingStreamReaderConf)
    {
      private var lastProblem: Option[Problem] = None
      override protected def idleTimeout = None  // SubagentEventListener itself detects heartbeat loss

      override protected def couple(eventId: EventId) =
        logger.debugTask(
          dedicateOrCouple
            .flatMapT { case (_, eventId) =>
              coupled.switchOn
                .as(Right(eventId))
            })
          .<*(Task {
            lastProblem = None
          })

      protected def getObservable(api: SubagentClient, after: EventId) =
        logger.debugTask(s"getObservable(after=$after)")(
          persistence.state.map(_.idToSubagentItemState.checked(subagentId).map(_.subagentRunId))
            .flatMapT {
              case None => Task.left(Problem.pure("Subagent not yet dedicated"))
              case Some(subagentRunId) => getObservable(api, after, subagentRunId)
            })

      private def getObservable(api: SubagentClient, after: EventId, subagentRunId: SubagentRunId) =
        api.login(onlyIfNotLoggedIn = true) *>
          api
            .eventObservable(
              EventRequest.singleClass[Event](after = after, timeout = None),
              subagentRunId,
              heartbeat = Some(heartbeatTiming.heartbeat))
            .map(_
              .detectPauses2(heartbeatTiming.longHeartbeatTimeout, PauseDetected)
              .flatTap(stamped =>
                if (stamped ne PauseDetected)
                  onHeartbeatStarted
                else {
                  val problem = Problem.pure(s"Missing heartbeat from $subagentId")
                  logger.warn(problem.toString)
                  Observable.fromTask(onSubagentDecoupled(Some(problem)))
                })
              .filter(_ != StampedHeartbeat)
              .takeWhile(_ ne PauseDetected)
              .guaranteeCase(exitCase => Task.defer {
                // guaranteeCase runs concurrently, maybe with onDecoupled ???
                Task.when((exitCase != ExitCase.Completed /*&& exitCase != ExitCase.Canceled*/) || /*isCoupled*/isHeartbeating)(
                  stopObserving
                    .flatMap(_.tryRead)
                    .map {
                      case None =>
                        val problem = exitCase match {
                          case ExitCase.Completed =>
                            Problem.pure(s"$subagentId event stream has ended")
                          case _ =>
                            Problem.pure(s"$subagentId event stream: $exitCase")
                        }
                        logger.warn(problem.toString)
                        problem
                      case Some(()) =>
                        Problem.pure("SubagentEventListener stopped")
                    }
                    .flatMap(problem => onSubagentDecoupled(Some(problem))))
              }))
            .map(Right(_))

      override protected def onCouplingFailed(api: SubagentClient, problem: Problem) =
        onSubagentDecoupled(Some(problem)) *>
          Task {
            if (lastProblem contains problem) {
              logger.debug(s"⚠️ Coupling failed: $problem")
            } else {
              lastProblem = Some(problem)
              logger.warn(s"Coupling failed: $problem")
            }
            true
          }

      override protected val onDecoupled =
        logger.traceTask(
          onSubagentDecoupled(None) *>
            coupled.switchOff.as(Completed))

      protected def stopRequested = false
    }

  private val onHeartbeatStarted: Observable[Unit] =
    Observable.fromTask(
      Task.defer(Task.when(!_isHeartbeating.getAndSet(true) && isCoupled)(
        // Different to AgentDriver,
        // for Subagents the Coupling state is tied to the continuous flow of events.
        persistence.persistKeyedEvent(subagentId <-: SubagentCoupled)
          .map(_.orThrow))))

  protected final def isHeartbeating = _isHeartbeating.get()

  private def onSubagentDecoupled(problem: Option[Problem]): Task[Unit] =
    Task.defer {
      _isHeartbeating := false
      Task.when(true || isCoupled)(
        emitSubagentCouplingFailed(problem))
    }
}

private object SubagentEventListener
{
  private val PauseDetected: Stamped[KeyedEvent[Event]] = Stamped(0L, null)
  private[subagent] val heartbeatTiming = HeartbeatTiming(3.s, 10.s)  // TODO
}
