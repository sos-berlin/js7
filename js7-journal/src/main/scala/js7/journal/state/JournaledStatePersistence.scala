package js7.journal.state

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked
import js7.base.utils.Assertions.assertThat
import js7.base.utils.SetOnce
import js7.common.akkautils.Akkas.encodeAsActorName
import js7.data.cluster.ClusterState
import js7.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import js7.journal.JournalActor
import js7.journal.configuration.JournalConf
import js7.journal.state.StateJournalingActor.{PersistFunction, StateToEvents}
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Promise
import scala.reflect.runtime.universe._
import shapeless.tag.@@

// TODO Lock for NoKey is to wide. Restrict to a set of Event superclasses, like ClusterEvent, ControllerEvent?
//  Der Aufrufer kann sich um die Sperren uns dessen Granularität kümmern.
//  JournaledStatePersistence stellt dazu LockKeeper bereit
//  Wir werden vielleicht mehrere Schlüssel auf einmal sperren wollen (für fork/join?)

final class JournaledStatePersistence[S <: JournaledState[S]](
  val/*???*/ journalActor: ActorRef @@ JournalActor.type,
  journalConf: JournalConf)
  (implicit S: TypeTag[S], s: Scheduler, actorRefFactory: ActorRefFactory, timeout: akka.util.Timeout)
extends AutoCloseable
{
  private val lockKeeper = new LockKeeper[Any]  // TODO Should the caller be responsible for sequential key updates? We could allow parallel, independent(!) updates
  import lockKeeper.lock
  private val persistPromise = Promise[PersistFunction[S, Event]]()
  private val persistTask: Task[PersistFunction[S, Event]] = Task.fromFuture(persistPromise.future)

  private val actorOnce = SetOnce[ActorRef]
  private val getCurrentState = SetOnce[() => S]

  def actor = actorOnce.orThrow

  def close(): Unit =
    actorOnce.foreach(actorRefFactory.stop)

  def start(state: S): Task[Unit] =
    Task {
      actorOnce := actorRefFactory.actorOf(
        StateJournalingActor.props[S, Event](state, journalActor, journalConf, persistPromise),
        encodeAsActorName("StateJournalingActor-" + S.tpe.toString))
    }

  // TODO Call start() only after JournalActor has been started
  // Then fill getCurrentState in start() and delete this method.
  def onJournalActorStarted: Task[Unit] =
    provideGetCurrentState

  def persistKeyedEvent[E <: Event](keyedEvent: KeyedEvent[E]): Task[Checked[(Stamped[KeyedEvent[E]], S)]] = {
    requireStarted()
    persistEvent(key = keyedEvent.key)(_ => Right(keyedEvent.event))
  }

  def persistEvent[E <: Event](key: E#Key): (S => Checked[E]) => Task[Checked[(Stamped[KeyedEvent[E]], S)]] = {
    requireStarted()
    stateToEvent => lock(key)(
      persistEventUnlocked(
        stateToEvent.andThen(_.map(KeyedEvent(key, _)))))
  }

  private def persistEventUnlocked[E <: Event](stateToEvent: S => Checked[KeyedEvent[E]]): Task[Checked[(Stamped[KeyedEvent[E]], S)]] =
    persistTask
      .flatMap(_(state => stateToEvent(state).map(_ :: Nil), /*transaction=*/false))
      .map(_ map {
        case (stampedKeyedEvents, state) =>
          assertThat(stampedKeyedEvents.lengthIs == 1)
          stampedKeyedEvents.head.asInstanceOf[Stamped[KeyedEvent[E]]] -> state
      })

  def persist[E <: Event]: (S => Checked[Seq[KeyedEvent[E]]]) => Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] = {
    requireStarted()
    stateToEvents => persistUnlocked(stateToEvents, transaction = false)
  }

  /** Persist multiple events in a transaction. */
  def persistTransaction[E <: Event](key: E#Key): (S => Checked[Seq[E]]) => Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] = {
    requireStarted()
    stateToEvents =>
      lock(key)(
        persistUnlocked(
          state => stateToEvents(state)
            .map(_.map(KeyedEvent[E](key, _))),
          transaction = true))
  }

  private def persistUnlocked[E <: Event](stateToEvents: StateToEvents[S, E], transaction: Boolean)
  : Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] = {
    requireStarted()
    persistTask.flatMap(
      _(stateToEvents, transaction)
        .map(_.map { case (stampedKeyedEvents, state) =>
          stampedKeyedEvents.asInstanceOf[Seq[Stamped[KeyedEvent[E]]]] -> state }))
  }

  def isStarted = actorOnce.nonEmpty

  private def requireStarted() =
    if (actorOnce.isEmpty) throw new IllegalStateException(s"$toString has not yet been started")

  def clusterState: Task[ClusterState] =
    awaitCurrentState.map(_.clusterState)

  private val provideGetCurrentState: Task[Unit] =
    (waitUntilStarted >>
      Task
        .deferFuture(
          (journalActor ? JournalActor.Input.GetJournaledState).mapTo[() => S])
        .logWhenItTakesLonger("JournalActor.Input.GetJournaledState")
        .map(getCurrentState := _)
        .as(())
    ).memoize

  def awaitCurrentState: Task[S] =
    provideGetCurrentState >>
      Task(currentState)

  def currentState: S =
    getCurrentState.orThrow()

  def waitUntilStarted: Task[Unit] =
    actorOnce.task
      .map(_ => ())
      .logWhenItTakesLonger(s"$toString.waitUntilStarted")

  override def toString = s"JournaledStatePersistence[${S.tpe}]"
}

object JournaledStatePersistence
{
  def start[S <: JournaledState[S]](
    initialState: S,
    journalActor: ActorRef @@ JournalActor.type,
    journalConf: JournalConf)
    (implicit S: TypeTag[S], s: Scheduler, actorRefFactory: ActorRefFactory, timeout: akka.util.Timeout)
  = Task.defer {
    val persistence = new JournaledStatePersistence[S](journalActor, journalConf)
    persistence
      .start(initialState)
      .tapEval(_ => persistence.onJournalActorStarted)
      .as(persistence)
  }
}
