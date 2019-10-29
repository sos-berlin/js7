package com.sos.jobscheduler.core.event.state

import akka.actor.{ActorRef, Props}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.common.scalautil.MonixUtils.promiseTask
import com.sos.jobscheduler.core.event.journal.MainJournalingActor
import com.sos.jobscheduler.core.event.state.StateJournalingActor._
import com.sos.jobscheduler.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import monix.eval.Task
import monix.execution.Scheduler
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
  // Will be accesses asynchronously via `getStatePromise`
  @volatile private var state: S = initialState

  protected def snapshots = state.toSnapshotObservable.toListL.runToFuture

  override def preStart() = {
    super.preStart()
    persistPromise.success(stateToEvent => persistStateToEvent(stateToEvent))
    getStatePromise.success(Task(state))
  }

  private def persistStateToEvent(stateToEvent: S => Checked[KeyedEvent[E]]): Task[Checked[(Stamped[KeyedEvent[E]], S)]] =
    promiseTask[Checked[(Stamped[KeyedEvent[E]], S)]] { promise =>
      self ! Persist(stateToEvent, promise)
    }

  def receive = {
    case Persist(stateToEvent, promise) =>
      val tried = Try(stateToEvent(state).flatMap(keyedEvent => state.applyEvent(keyedEvent).map(_ => keyedEvent)))
      tried match {
        case Failure(t) => promise.failure(t)
        case Success(Left(problem)) => promise.success(Left(problem))
        case Success(Right(keyedEvent)) =>
          promise.completeWith(
            persistKeyedEvent(keyedEvent, async = true) { stamped =>
              Right(onPersisted(stamped))
            })
      }
  }

  private def onPersisted(stamped: Stamped[KeyedEvent[E]]): (Stamped[KeyedEvent[E]], S) =
    state.withEventId(stamped.eventId)
      .applyEvent(stamped.value) match {
        case Left(problem) => throw new IllegalStateChangeWhilePersistingException(stamped, problem)  // Serious problem !!!
        case Right(updated) =>
          state = updated
          stamped -> updated
      }

  override lazy val toString = s"StateJournalingActor[${S.tpe.toString.replaceAll("""^.*\.""", "")}]"

  private case class Persist(stateToEvent: S => Checked[KeyedEvent[E]], promise: Promise[Checked[(Stamped[KeyedEvent[E]], S)]])
}

private[state] object StateJournalingActor
{
  type PersistFunction[S <: JournaledState[S, E], E <: Event] = (S => Checked[KeyedEvent[E]]) => Task[Checked[(Stamped[KeyedEvent[E]], S)]]

  def props[S <: JournaledState[S, E], E <: Event](
    initialState: S,
    journalActor: ActorRef,
    persistPromise: Promise[PersistFunction[S, E]],
    getStatePromise: Promise[Task[S]])
    (implicit S: TypeTag[S], s: Scheduler)
  =
    Props { new StateJournalingActor(initialState, journalActor, persistPromise, getStatePromise) }

  private final class IllegalStateChangeWhilePersistingException(stamped: Stamped[KeyedEvent[_]], problem: Problem)
  extends RuntimeException(s"Application of event failed after persisted: $stamped: $problem")
}
