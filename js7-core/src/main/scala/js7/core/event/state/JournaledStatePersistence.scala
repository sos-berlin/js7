package js7.core.event.state

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import js7.base.monixutils.MonixBase.deferFutureAndLog
import js7.base.problem.Checked
import js7.base.utils.Assertions.assertThat
import js7.base.utils.SetOnce
import js7.common.akkautils.Akkas.encodeAsActorName
import js7.common.scalautil.Logger
import js7.core.event.journal.{JournalActor, JournalConf}
import js7.core.event.state.JournaledStatePersistence._
import js7.core.event.state.StateJournalingActor.{PersistFunction, StateToEvents}
import js7.data.cluster.ClusterState
import js7.data.event.{Event, JournaledState, KeyedEvent, Stamped}
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
  private val persistPromise = Promise[PersistFunction[S, Event]]()
  private val persistTask: Task[PersistFunction[S, Event]] = Task.fromFuture(persistPromise.future)

  private val actorOnce = SetOnce[ActorRef]

  def actor = actorOnce.orThrow

  def close(): Unit =
    actorOnce.foreach(actorRefFactory.stop)

  def start(state: S): Unit =
    actorOnce := actorRefFactory.actorOf(
      StateJournalingActor.props[S, Event](state, journalActor, journalConf, persistPromise),
      encodeAsActorName("StateJournalingActor-" + S.tpe.toString))

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
    persistTask.flatMap(
      _(state => stateToEvent(state).map(_ :: Nil))
    ).map(_ map {
      case (stampedKeyedEvents, state) =>
        assertThat(stampedKeyedEvents.lengthIs == 1)
        stampedKeyedEvents.head.asInstanceOf[Stamped[KeyedEvent[E]]] -> state
    })

  /** Persist multiple events in a transaction. */
  def persistTransaction[E <: Event](key: E#Key): (S => Checked[Seq[E]]) => Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] = {
    requireStarted()
    stateToEvents =>
      lock(key)(
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

  private def lock[A](key: Any)(body: Task[A]): Task[A] =
    lockKeeper.lock(key).use(_ => body)

  def isStarted = actorOnce.nonEmpty

  private def requireStarted() =
    if (actorOnce.isEmpty) throw new IllegalStateException(s"$toString has not yet been started")

  def clusterState: Task[ClusterState] =
    currentState.map(_.clusterState)

  def currentState: Task[S] =
    waitUntilStarted >>
    Task.deferFuture {
      logger.debug(s"JournalActor.Input.GetJournaledState")
      (journalActor ? JournalActor.Input.GetJournaledState).mapTo[JournaledState[S]]
    }.map(_.asInstanceOf[S])

  def waitUntilStarted: Task[Unit] =
    deferFutureAndLog(actorOnce.future, s"$toString.waitUntilStarted")
      .map(_ => ())

  override def toString = s"JournaledStatePersistence[${S.tpe}]"
}

object JournaledStatePersistence {
  private val logger = Logger(getClass)
}
