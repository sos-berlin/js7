package js7.journal.state

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import akka.util.Timeout
import com.softwaremill.diffx
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.SetOnce
import js7.common.akkautils.Akkas.encodeAsActorName
import js7.data.cluster.ClusterState
import js7.data.event.{Event, JournalHeader, JournalHeaders, JournalId, JournaledState, KeyedEvent, Stamped}
import js7.journal.configuration.JournalConf
import js7.journal.data.JournalMeta
import js7.journal.recover.Recovered
import js7.journal.state.JournaledStatePersistence.logger
import js7.journal.state.StateJournalingActor.{PersistFunction, StateToEvents}
import js7.journal.watch.EventWatch
import js7.journal.{EventIdGenerator, JournalActor, StampedKeyedEventBus}
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.{Future, Promise}
import scala.reflect.runtime.universe._
import shapeless.tag
import shapeless.tag.@@

// TODO Lock for NoKey is to wide. Restrict to a set of Event superclasses, like ClusterEvent, ControllerEvent?
//  Der Aufrufer kann sich um die Sperren uns dessen Granularität kümmern.
//  JournaledStatePersistence stellt dazu LockKeeper bereit
//  Wir werden vielleicht mehrere Schlüssel auf einmal sperren wollen (für fork/join?)

final class JournaledStatePersistence[S <: JournaledState[S]](
  val recoveredJournalId: Option[JournalId],
  val eventWatch: EventWatch,
  val journalActor: ActorRef @@ JournalActor.type,
  journalConf: JournalConf,
  journalActorStopped: Future[Unit])
  (implicit S: TypeTag[S], s: Scheduler, actorRefFactory: ActorRefFactory, timeout: akka.util.Timeout)
extends AutoCloseable
{
  lazy val journalId = recoveredJournalId getOrElse JournalId.random()

  private val lockKeeper = new LockKeeper[Any]  // TODO Should the caller be responsible for sequential key updates? We could allow parallel, independent(!) updates
  private val persistPromise = Promise[PersistFunction[S, Event]]()
  private val persistTask: Task[PersistFunction[S, Event]] = Task.fromFuture(persistPromise.future)

  private val actorOnce = SetOnce[ActorRef]
  private val getCurrentState = SetOnce[() => S]
  private val journalHeaderOnce = SetOnce[JournalHeader]

  def journalHeader: Task[JournalHeader] =
    journalHeaderOnce.task

  def actor = actorOnce.orThrow

  def close(): Unit =
    stop.runAsyncAndForget

  def start(recovered: Recovered[S]): Task[JournalHeader] =
    Task.defer {
      if (recovered.recoveredJournalFile.isEmpty) {
        logger.info("Starting a new empty journal")
      }
      val start = JournalActor.Input.Start(
        recovered.state,
        Some(recovered.eventWatch),
        recovered.recoveredJournalFile.fold(JournalHeaders.initial(journalId))(_.nextJournalHeader),
        recovered.totalRunningSince)
      Task.fromFuture((journalActor ? start)(Timeout(1.h/*???*/)).mapTo[JournalActor.Output.Ready])
        .flatMap { case JournalActor.Output.Ready(journalHeader) =>
          logger.debug(s"JournalIsReady")
          journalHeaderOnce := journalHeader
          Task
            .deferFuture(
              (journalActor ? JournalActor.Input.GetJournaledState).mapTo[() => S])
            .logWhenItTakesLonger("JournalActor.Input.GetJournaledState")
            .map { getCurrentState := _ }
            .as(journalHeader)
        }
        .tapEval(_ => Task {
          actorOnce := actorRefFactory.actorOf(
            StateJournalingActor.props[S, Event](currentState, journalActor, journalConf, persistPromise),
            encodeAsActorName("StateJournalingActor:" + S.tpe.toString))
        })
    }

  val stop: Task[Unit] =
    Task.defer {
      actorOnce foreach actorRefFactory.stop
      journalActor ! JournalActor.Input.Terminate
      Task.fromFuture(journalActorStopped)
    }.memoize

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

  private def persistEventUnlocked[E <: Event](stateToEvent: S => Checked[KeyedEvent[E]])
  : Task[Checked[(Stamped[KeyedEvent[E]], S)]] =
    persistTask
      .flatMap(_(state => stateToEvent(state).map(_ :: Nil), /*transaction=*/false))
      .map(_ map {
        case (stampedKeyedEvents, state) =>
          assertThat(stampedKeyedEvents.lengthIs == 1)
          stampedKeyedEvents.head.asInstanceOf[Stamped[KeyedEvent[E]]] -> state
      })

  def persist[E <: Event](stateToEvents: S => Checked[Seq[KeyedEvent[E]]]): Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] = {
    requireStarted()
    persistUnlocked(stateToEvents, transaction = false)
  }

  /** Persist multiple events in a transaction. */
  def persistTransaction[E <: Event](key: E#Key)
  : (S => Checked[Seq[E]]) => Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] = {
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

  def awaitCurrentState: Task[S] =
    waitUntilStarted >> Task(currentState)

  def currentState: S =
    getCurrentState.orThrow()

  def waitUntilStarted: Task[Unit] =
    actorOnce.task
      .map(_ => ())
      .logWhenItTakesLonger(s"$toString.waitUntilStarted")

  def lock[A](key: Any)(body: Task[A]): Task[A] =
    lockKeeper.lock(key)(body)

  override def toString = s"JournaledStatePersistence[${S.tpe}]"
}

object JournaledStatePersistence
{
  private val logger = Logger[this.type]

  def start[S <: JournaledState[S]: JournaledState.Companion: diffx.Diff](
    recovered: Recovered[S],
    journalMeta: JournalMeta,
    journalConf: JournalConf,
    eventIdGenerator: EventIdGenerator = new EventIdGenerator,
    keyedEventBus: StampedKeyedEventBus = new StampedKeyedEventBus)
    (implicit S: TypeTag[S], scheduler: Scheduler,
      actorRefFactory: ActorRefFactory, timeout: akka.util.Timeout)
  : Task[JournaledStatePersistence[S]] = {
    val persistence = prepare(recovered.journalId, recovered.eventWatch, journalMeta, journalConf,
      eventIdGenerator, keyedEventBus)
    persistence.start(recovered)
      .as(persistence)
  }

  def prepare[S <: JournaledState[S]: JournaledState.Companion: diffx.Diff](
    recoveredJournalId: Option[JournalId],
    eventWatch: EventWatch,
    journalMeta: JournalMeta,
    journalConf: JournalConf,
    eventIdGenerator: EventIdGenerator = new EventIdGenerator,
    keyedEventBus: StampedKeyedEventBus = new StampedKeyedEventBus)
    (implicit S: TypeTag[S], scheduler: Scheduler,
      actorRefFactory: ActorRefFactory, timeout: akka.util.Timeout)
  : JournaledStatePersistence[S] = {
      val journalActorStopped = Promise[JournalActor.Stopped]()
      val journalActor = tag[JournalActor.type](actorRefFactory.actorOf(
        JournalActor.props[S](journalMeta, journalConf, keyedEventBus, scheduler, eventIdGenerator, journalActorStopped),
        "Journal"))

      new JournaledStatePersistence[S](recoveredJournalId, eventWatch, journalActor, journalConf,
        journalActorStopped.future.map(_ => ()))
    }
}
