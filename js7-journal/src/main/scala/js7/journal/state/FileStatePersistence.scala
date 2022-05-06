package js7.journal.state

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import akka.util.Timeout
import com.softwaremill.diffx
import js7.base.log.CorrelIdBinder.currentCorrelId
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.SetOnce
import js7.common.akkautils.Akkas.encodeAsActorName
import js7.data.cluster.ClusterState
import js7.data.event.{Event, JournalHeader, JournalHeaders, JournalId, KeyedEvent, SnapshotableState, Stamped}
import js7.journal.configuration.JournalConf
import js7.journal.data.JournalMeta
import js7.journal.recover.Recovered
import js7.journal.state.FileStatePersistence.logger
import js7.journal.state.StateJournalingActor.{PersistFunction, PersistLaterFunction, StateToEvents}
import js7.journal.watch.FileEventWatch
import js7.journal.{CommitOptions, EventIdGenerator, JournalActor, StampedKeyedEventBus}
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.{Future, Promise}
import scala.reflect.runtime.universe._
import shapeless.tag
import shapeless.tag.@@

// TODO Lock for NoKey is to wide. Restrict to a set of Event superclasses, like ClusterEvent, ControllerEvent?
//  Der Aufrufer kann sich um die Sperren uns dessen Granularität kümmern.
//  FileStatePersistence stellt dazu LockKeeper bereit
//  Wir werden vielleicht mehrere Schlüssel auf einmal sperren wollen (für fork/join?)

final class FileStatePersistence[S <: SnapshotableState[S]: TypeTag](
  val recoveredJournalId: Option[JournalId],
  val eventWatch: FileEventWatch,
  val journalActor: ActorRef @@ JournalActor.type,
  journalConf: JournalConf,
  journalActorStopped: Future[Unit])
  (implicit
    protected val S: SnapshotableState.Companion[S],
    scheduler: Scheduler,
    actorRefFactory: ActorRefFactory,
    timeout: akka.util.Timeout)
extends StatePersistence[S] with AutoCloseable
{
  lazy val journalId = recoveredJournalId getOrElse JournalId.random()

  private val persistPromise = Promise[PersistFunction[S, Event]]()
  private val persistTask: Task[PersistFunction[S, Event]] = Task.fromFuture(persistPromise.future)

  private val persistLaterPromise = Promise[PersistLaterFunction[Event]]()
  private val persistLaterTask: Task[PersistLaterFunction[Event]] =
    Task.fromFuture(persistLaterPromise.future)

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
            StateJournalingActor.props[S, Event](currentState, journalActor, journalConf,
              persistPromise, persistLaterPromise),
            encodeAsActorName("StateJournalingActor:" + S))
        })
    }

  val stop: Task[Unit] =
    Task.defer {
      actorOnce foreach actorRefFactory.stop
      journalActor ! JournalActor.Input.Terminate
      Task.fromFuture(journalActorStopped)
    }.memoize

  def persistKeyedEvent[E <: Event](
    keyedEvent: KeyedEvent[E],
    options: CommitOptions = CommitOptions.default)
    (implicit enclosing: sourcecode.Enclosing)
  : Task[Checked[(Stamped[KeyedEvent[E]], S)]] =
    Task.defer {
      requireStarted()
      persistEvent(key = keyedEvent.key, options)(enclosing)(_ => Right(keyedEvent.event))
    }

  def persistKeyedEvents[E <: Event](
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    persistWithOptions(options, _ => Right(keyedEvents))

  def persistKeyedEventsLater[E <: Event](
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : Task[Checked[Unit]] =
    persistLaterTask.flatMap(_(keyedEvents, options))

  def persistEvent[E <: Event](key: E#Key, options: CommitOptions = CommitOptions.default)
    (implicit enclosing: sourcecode.Enclosing)
  : (S => Checked[E]) => Task[Checked[(Stamped[KeyedEvent[E]], S)]] =
    stateToEvent => Task.defer {
      requireStarted()
      lock(key)(
        persistEventUnlocked(
          stateToEvent.andThen(_.map(KeyedEvent(key, _))),
          options))
    }

  private def persistEventUnlocked[E <: Event](
    stateToEvent: S => Checked[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : Task[Checked[(Stamped[KeyedEvent[E]], S)]] =
    persistTask
      .flatMap(_(state => stateToEvent(state).map(_ :: Nil), options, currentCorrelId))
      .map(_ map {
        case (stampedKeyedEvents, state) =>
          assertThat(stampedKeyedEvents.lengthIs == 1)
          stampedKeyedEvents.head.asInstanceOf[Stamped[KeyedEvent[E]]] -> state
      })

  def persistWithOptions[E <: Event](
    options: CommitOptions = CommitOptions.default,
    stateToEvents: S => Checked[Seq[KeyedEvent[E]]])
  : Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    Task.defer {
      requireStarted()
      persistUnlocked(stateToEvents, options)
    }

  /** Persist multiple events in a transaction. */
  def persistTransaction[E <: Event](key: E#Key)
  : (S => Checked[Seq[E]]) => Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    stateToEvents => Task.defer {
      requireStarted()
      lock(key)(
        persistUnlocked(
          state => stateToEvents(state)
            .map(_.map(KeyedEvent[E](key, _))),
          CommitOptions(transaction = true)))
    }

  private def persistUnlocked[E <: Event](
    stateToEvents: StateToEvents[S, E],
    options: CommitOptions)
  : Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    Task.defer {
      requireStarted()
      persistTask.flatMap(
        _(stateToEvents, options, currentCorrelId)
          .map(_.map { case (stampedKeyedEvents, state) =>
            stampedKeyedEvents.asInstanceOf[Seq[Stamped[KeyedEvent[E]]]] -> state }))
    }

  def isStarted = actorOnce.nonEmpty

  private def requireStarted() =
    if (actorOnce.isEmpty) throw new IllegalStateException(s"$toString has not yet been started")

  def clusterState: Task[ClusterState] =
    awaitCurrentState.map(_.clusterState)

  def currentState: S =
    getCurrentState.orThrow()

  def waitUntilStarted: Task[Unit] =
    actorOnce.task
      .map(_ => ())
      .logWhenItTakesLonger(s"$toString.waitUntilStarted")

  override def toString = s"FileStatePersistence[$S]"
}

object FileStatePersistence
{
  private val logger = Logger[this.type]

  def start[S <: SnapshotableState[S]: SnapshotableState.Companion: diffx.Diff: TypeTag](
    recovered: Recovered[S],
    journalConf: JournalConf,
    eventIdGenerator: EventIdGenerator = new EventIdGenerator,
    keyedEventBus: StampedKeyedEventBus = new StampedKeyedEventBus)
    (implicit
      scheduler: Scheduler,
      actorRefFactory: ActorRefFactory,
      timeout: akka.util.Timeout)
  : Task[FileStatePersistence[S]] = {
    import recovered.journalMeta
    val persistence = prepare(recovered.journalId, recovered.eventWatch, journalMeta, journalConf,
      eventIdGenerator, keyedEventBus)
    persistence.start(recovered)
      .as(persistence)
  }

  def prepare[S <: SnapshotableState[S]: SnapshotableState.Companion: diffx.Diff: TypeTag](
    recoveredJournalId: Option[JournalId],
    eventWatch: FileEventWatch,
    journalMeta: JournalMeta,
    journalConf: JournalConf,
    eventIdGenerator: EventIdGenerator = new EventIdGenerator,
    keyedEventBus: StampedKeyedEventBus = new StampedKeyedEventBus)
    (implicit
      scheduler: Scheduler,
      actorRefFactory: ActorRefFactory,
      timeout: akka.util.Timeout)
  : FileStatePersistence[S] = {
    val journalActorStopped = Promise[JournalActor.Stopped]()
    val journalActor = tag[JournalActor.type](actorRefFactory.actorOf(
      JournalActor.props[S](journalMeta, journalConf, keyedEventBus, scheduler, eventIdGenerator, journalActorStopped),
      "Journal"))

    new FileStatePersistence[S](recoveredJournalId, eventWatch, journalActor, journalConf,
      journalActorStopped.future.map(_ => ()))
  }
}
