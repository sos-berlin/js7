package com.sos.jobscheduler.core.event.state

import akka.actor.{ActorRef, Props}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.common.akkautils.SupervisorStrategies
import com.sos.jobscheduler.common.scalautil.MonixUtils.promiseTask
import com.sos.jobscheduler.core.event.journal.MainJournalingActor
import com.sos.jobscheduler.core.event.state.StateJournalingActor._
import com.sos.jobscheduler.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import monix.eval.Task
import monix.execution.Scheduler
import scala.collection.immutable.Seq
import scala.concurrent.Promise
import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}

private[state] final class StateJournalingActor[S <: JournaledState[S, E], E <: Event](
  initialState: S,
  protected val journalActor: ActorRef,
  persistPromise: Promise[PersistFunction[S, E]],
  getStatePromise: Promise[Task[S]])
  (implicit S: TypeTag[S], s: Scheduler)
extends MainJournalingActor[E]
{
  override def supervisorStrategy = SupervisorStrategies.escalate

  // Will be accesses asynchronously via `getStatePromise`
  @volatile private var state: S = initialState

  protected def snapshots = state.toSnapshotObservable.toListL.runToFuture

  override def preStart() = {
    super.preStart()
    persistPromise.success(stateToEvent => persistStateToEvents(stateToEvent))
    getStatePromise.success(Task { state })
  }

  private def persistStateToEvents(stateToEvents: StateToEvents[S, E]): Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    promiseTask[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] { promise =>
      self ! Persist(stateToEvents, promise)
    }

  def receive = {
    case Persist(stateToEvent, promise) =>
      val tried = Try(stateToEvent(state).flatMap(keyedEvent => state.applyEvents(keyedEvent).map(_ => keyedEvent)))
      tried match {
        case Failure(t) => promise.failure(t)
        case Success(Left(problem)) => promise.success(Left(problem))
        case Success(Right(keyedEvents)) =>
          promise.completeWith(
            persistKeyedEvents(toTimestamped(keyedEvents), transaction = true, async = true) { stampedKeyedEvents =>
              val updated = applyPersistedEvents(stampedKeyedEvents)
              state = updated
              Right(stampedKeyedEvents -> updated)
            })
      }
  }

  private def applyPersistedEvents(stampedKeyedEvents: Seq[Stamped[KeyedEvent[E]]]): S =
    stampedKeyedEvents.scanLeft(state)(applyPersistedEvent[S, E]).last

  override lazy val toString = s"StateJournalingActor[${S.tpe.toString.replaceAll("""^.*\.""", "")}]"

  private case class Persist(stateToEvents: StateToEvents[S, E], promise: Promise[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]])
}

private[state] object StateJournalingActor
{
  type StateToEvents[S <: JournaledState[S, E], E <: Event] = S => Checked[Seq[KeyedEvent[E]]]
  type PersistFunction[S <: JournaledState[S, E], E <: Event] = StateToEvents[S, E] => Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]]

  def props[S <: JournaledState[S, E], E <: Event](
    initialState: S,
    journalActor: ActorRef,
    persistPromise: Promise[PersistFunction[S, E]],
    getStatePromise: Promise[Task[S]])
    (implicit S: TypeTag[S], s: Scheduler)
  =
    Props { new StateJournalingActor(initialState, journalActor, persistPromise, getStatePromise) }

  private def applyPersistedEvent[S <: JournaledState[S, E], E <: Event] (state: S, stampedKeyedEvent: Stamped[KeyedEvent[E]]): S =
    state.withEventId(stampedKeyedEvent.eventId)
      .applyEvent(stampedKeyedEvent.value) match {
        case Left(problem) => throw new IllegalStateChangeWhilePersistingException(stampedKeyedEvent, problem)  // Serious problem !!!
        case Right(updated) => updated
      }

  private final class IllegalStateChangeWhilePersistingException(stamped: Stamped[KeyedEvent[_]], problem: Problem)
  extends RuntimeException(s"Application of event failed after persisted: $stamped: $problem")
}
