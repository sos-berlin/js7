package com.sos.jobscheduler.core.event.state

import akka.actor.{ActorRef, ActorRefFactory}
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.core.event.journal.JournalActor
import com.sos.jobscheduler.core.event.state.StateJournalingActor.PersistFunction
import com.sos.jobscheduler.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Promise
import scala.language.higherKinds
import scala.reflect.runtime.universe._
import shapeless.tag.@@

final class JournaledStatePersistence[S <: JournaledState[S, E], E <: Event](
  initialState: S,
  val/*???*/ journalActor: ActorRef @@ JournalActor.type)
  (implicit S: TypeTag[S], s: Scheduler, actorRefFactory: ActorRefFactory)
{
  private val lockKeeper = new LockKeeper[E#Key]  // TODO Should the caller be responsible for sequential key updates? We could allow parallel, independent(!) updates
  private val persistPromise = Promise[PersistFunction[S, E]]()
  private val getStatePromise = Promise[Task[S]]()
  private val persistTask: Task[PersistFunction[S, E]] = Task.fromFuture(persistPromise.future)
  private val getStateTask: Task[S] = Task.fromFuture(getStatePromise.future).flatten

  actorRefFactory.actorOf(
    StateJournalingActor.props[S, E](initialState, journalActor, persistPromise, getStatePromise))

  def persistKeyedEvent(keyedEvent: KeyedEvent[E]): Task[Checked[(Stamped[KeyedEvent[E]], S)]] =
    persistEvent(key = keyedEvent.key, _ => Right(keyedEvent.event))

  /** `E1` is derived from argument `stateToEvent`. */
  def persistEvent[E1 <: E](stateToEvent: S => Checked[E1])(key: E1#Key): Task[Checked[(Stamped[KeyedEvent[E1]], S)]] =
    persistEvent[E1](key, stateToEvent)

  /** `E1` must be given explicitly. */
  def persistEvent[E1 <: E](key: E1#Key, stateToEvent: S => Checked[E1]): Task[Checked[(Stamped[KeyedEvent[E1]], S)]] =
    lockKeeper.lock(key).use(_ =>
      persistEventRaw(
        stateToEvent.andThen(_.map(KeyedEvent(key, _)))))

  private def persistEventRaw[E1 <: E](stateToEvent: S => Checked[KeyedEvent[E1]]): Task[Checked[(Stamped[KeyedEvent[E1]], S)]] =
    persistTask.flatMap(
      _(stateToEvent)
        .map(_.map { case (e, s) => e.asInstanceOf[Stamped[KeyedEvent[E1]]] -> s }))

  private[state] def currentState: Task[S] =
    getStateTask
}
