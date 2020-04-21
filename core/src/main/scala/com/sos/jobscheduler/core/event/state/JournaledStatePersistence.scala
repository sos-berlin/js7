package com.sos.jobscheduler.core.event.state

import akka.actor.{ActorRef, ActorRefFactory}
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.utils.SetOnce
import com.sos.jobscheduler.common.akkautils.Akkas.encodeAsActorName
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.event.journal.JournalActor
import com.sos.jobscheduler.core.event.state.JournaledStatePersistence._
import com.sos.jobscheduler.core.event.state.StateJournalingActor.{PersistFunction, StateToEvents}
import com.sos.jobscheduler.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Promise
import scala.reflect.runtime.universe._
import shapeless.tag.@@

// TODO Lock for NoKey is to wide. Restrict to a set of Event superclasses, like ClusterEvent, MasterEvent?
//  Der Aufrufer kann sich im die Sperren uns dessen Granularität kümmern.
//  JournaledStatePersistence stellt dazu LockKeeper bereit
//  Wir werden vielleicht mehrere Schlüssel auf einmal sperren wollen (für fork/join?)

final class JournaledStatePersistence[S <: JournaledState[S]](
  val/*???*/ journalActor: ActorRef @@ JournalActor.type)
  (implicit S: TypeTag[S], s: Scheduler, actorRefFactory: ActorRefFactory)
extends AutoCloseable
{
  private val lockKeeper = new LockKeeper[Any]  // TODO Should the caller be responsible for sequential key updates? We could allow parallel, independent(!) updates
  private val persistPromise = Promise[PersistFunction[S, Event]]()
  private val getStatePromise = Promise[Task[S]]()
  private val persistTask: Task[PersistFunction[S, Event]] = Task.fromFuture(persistPromise.future)
  val currentState: Task[S] = Task.fromFuture(getStatePromise.future).flatten

  private val actorSetOnce = SetOnce[ActorRef]

  def actor = actorSetOnce.orThrow

  def close(): Unit =
    actorSetOnce.foreach(actorRefFactory.stop)

  def start(state: S): Unit =
    actorSetOnce := actorRefFactory.actorOf(
      StateJournalingActor.props[S, Event](state, journalActor, persistPromise, getStatePromise),
      encodeAsActorName("StateJournalingActor-" + S.tpe.toString))

  def persistKeyedEvent[E <: Event](keyedEvent: KeyedEvent[E]): Task[Checked[(Stamped[KeyedEvent[E]], S)]] = {
    requireStarted()
    persistEvent(key = keyedEvent.key)(_ => Right(keyedEvent.event))
  }

  def persistEvent[E <: Event](key: E#Key): (S => Checked[E]) => Task[Checked[(Stamped[KeyedEvent[E]], S)]] = {
    requireStarted()
    stateToEvent => lockKeeper.lock(key).use(_ =>
      persistEventUnlocked(
        stateToEvent.andThen(_.map(KeyedEvent(key, _)))))
  }

  private def persistEventUnlocked[E <: Event](stateToEvent: S => Checked[KeyedEvent[E]]): Task[Checked[(Stamped[KeyedEvent[E]], S)]] =
    persistTask.flatMap(
      _(state => stateToEvent(state).map(_ :: Nil))
    ).map(_ map {
      case (stampedKeyedEvents, state) =>
        assertThat(stampedKeyedEvents.length == 1)
        stampedKeyedEvents.head.asInstanceOf[Stamped[KeyedEvent[E]]] -> state
    })

  /** Persist multiple events in a transaction. */
  def persistTransaction[E <: Event](key: E#Key): (S => Checked[Seq[E]]) => Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] = {
    requireStarted()
    stateToEvents =>
      lockKeeper.lock(key).use(_ =>
        persistTransactionUnlocked(state =>
          stateToEvents(state)
            .map(_.map(KeyedEvent[E](key, _)))))
  }

  private def persistTransactionUnlocked[E <: Event](stateToEvents: StateToEvents[S, E]): Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] = {
    requireStarted()
    persistTask.flatMap(
      _(stateToEvents)
        .map(_.map { case (stampedKeyedEvents, state) =>
          stampedKeyedEvents.asInstanceOf[Seq[Stamped[KeyedEvent[E]]]] -> state }))
  }

  private def requireStarted() =
    if (actorSetOnce.isEmpty) throw new IllegalStateException(s"$toString has not yet been started")

  def waitUntilStarted: Task[JournaledStatePersistence[S]] =
    Task.deferFuture {
      val f = actorSetOnce.future
      if (!f.isCompleted) logger.debug(s"$toString waitUntilStarted ...")
      f
    }.map(_ => this)

  override def toString = s"JournaledStatePersistence[${S.tpe}]"
}

object JournaledStatePersistence
{
  private val logger = Logger(getClass)
}
