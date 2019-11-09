package com.sos.jobscheduler.core.event.state

import akka.actor.{ActorRef, ActorRefFactory}
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.common.akkautils.Akkas.encodeAsActorName
import com.sos.jobscheduler.common.scalautil.SetOnce
import com.sos.jobscheduler.core.event.journal.JournalActor
import com.sos.jobscheduler.core.event.state.StateJournalingActor.{PersistFunction, StateToEvents}
import com.sos.jobscheduler.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import monix.eval.Task
import monix.execution.Scheduler
import scala.collection.immutable.Seq
import scala.concurrent.Promise
import scala.language.higherKinds
import scala.reflect.runtime.universe._
import shapeless.tag.@@

// TODO Lock for NoKey is to wide. Restrict to a set of Event superclasses, like ClusterEvent, MasterEvent?
//  Der Aufrufer kann sich im die Sperren uns dessen Granularität kümmern.
//  JournaledStatePersistence stellt dazu LockKeeper bereit
//  Wir werden vielleicht mehrere Schlüssel auf einmal sperren wollen (für fork/join?)

final class JournaledStatePersistence[S <: JournaledState[S, E], E <: Event](
  val/*???*/ journalActor: ActorRef @@ JournalActor.type)
  (implicit S: TypeTag[S], s: Scheduler, actorRefFactory: ActorRefFactory)
extends AutoCloseable
{
  private val lockKeeper = new LockKeeper[E#Key]  // TODO Should the caller be responsible for sequential key updates? We could allow parallel, independent(!) updates
  private val persistPromise = Promise[PersistFunction[S, E]]()
  private val getStatePromise = Promise[Task[S]]()
  private val persistTask: Task[PersistFunction[S, E]] = Task.fromFuture(persistPromise.future)
  val currentState: Task[S] = Task.fromFuture(getStatePromise.future).flatten

  private val actor = SetOnce[ActorRef]

  def close(): Unit =
    actor.foreach(actorRefFactory.stop)

  def start(state: S): Unit =
    actor := actorRefFactory.actorOf(
      StateJournalingActor.props[S, E](state, journalActor, persistPromise, getStatePromise),
      encodeAsActorName("StateJournalingActor-" + S.tpe.toString))

  def persistKeyedEvent[E1 <: E](keyedEvent: KeyedEvent[E1]): Task[Checked[(Stamped[KeyedEvent[E1]], S)]] = {
    requireStarted()
    persistEvent(key = keyedEvent.key)(_ => Right(keyedEvent.event))
  }

  def persistEvent[E1 <: E](key: E1#Key): (S => Checked[E1]) => Task[Checked[(Stamped[KeyedEvent[E1]], S)]] = {
    requireStarted()
    stateToEvent => lockKeeper.lock(key).use(_ =>
      persistEventUnlocked(
        stateToEvent.andThen(_.map(KeyedEvent(key, _)))))
  }

  private def persistEventUnlocked[E1 <: E](stateToEvent: S => Checked[KeyedEvent[E1]]): Task[Checked[(Stamped[KeyedEvent[E1]], S)]] =
    persistTask.flatMap(
      _(state => stateToEvent(state).map(_ :: Nil))
    ).map(_ map {
      case (stampedKeyedEvents, state) =>
        assertThat(stampedKeyedEvents.length == 1)
        stampedKeyedEvents.head.asInstanceOf[Stamped[KeyedEvent[E1]]] -> state
    })

  /** Persist multiple events in a transaction. */
  def persistTransaction(key: E#Key): (S => Checked[Seq[E]]) => Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] = {
    requireStarted()
    stateToEvents =>
      lockKeeper.lock(key).use(_ =>
        persistTransactionUnlocked(state =>
          stateToEvents(state)
            .map(_.map(KeyedEvent[E](key, _)))))
  }

  private def persistTransactionUnlocked(stateToEvents: StateToEvents[S, E]): Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] = {
    requireStarted()
    persistTask.flatMap(
      _(stateToEvents)
        .map(_.map { case (stampedKeyedEvents, state) =>
          stampedKeyedEvents.asInstanceOf[Seq[Stamped[KeyedEvent[E]]]] -> state }))
  }

  private def requireStarted() =
    if (actor.isEmpty) throw new IllegalStateException(s"$toString has not yet been started")

  override def toString = s"JournaledStatePersistence[${S.tpe}]"
}
