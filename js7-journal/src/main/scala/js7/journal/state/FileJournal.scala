package js7.journal.state

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource, ResourceIO}
import com.softwaremill.tagging.@@
import izumi.reflect.Tag
import js7.base.monixutils.Switch
import js7.base.problem.Checked
import js7.base.utils.Assertions.assertThat
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.data.cluster.ClusterState
import js7.data.event.{Event, EventCalc, JournalId, KeyedEvent, SnapshotableState, Stamped, TimeCtx}
import js7.journal.Journaler.Persist
import js7.journal.configuration.JournalConf
import js7.journal.recover.Recovered
import js7.journal.watch.FileEventWatch
import js7.journal.{CommitOptions, EventIdGenerator, JournalActor, Journaler}
import org.apache.pekko
import org.apache.pekko.actor.{ActorRef, ActorRefFactory}
import sourcecode.Enclosing

// TODO Lock for NoKey is to wide. Restrict to a set of Event superclasses, like ClusterEvent, ControllerEvent?
//  Der Aufrufer kann sich um die Sperren uns dessen Granularität kümmern.
//  FileJournal stellt dazu LockKeeper bereit
//  Wir werden vielleicht mehrere Schlüssel auf einmal sperren wollen (für fork/join?)

final class FileJournal[S <: SnapshotableState[S]: Tag] private(
  val journalConf: JournalConf,
  val eventWatch: FileEventWatch,
  val journaler: Journaler[S],
  val journalActor: ActorRef @@ JournalActor.type)
  (implicit
    protected val S: SnapshotableState.Companion[S])
extends Journal[S], FileJournal.PossibleFailover:

  def journalId = journaler.journalId
  def totalRunningTime = journaler.totalRunningTime
  def initiallyStartedAt = journaler.initiallyStartedAt

  def isHalted: Boolean =
    journaler.isHalted

  def persistKeyedEvent[E <: Event](keyedEvent: KeyedEvent[E])
    (using enclosing: sourcecode.Enclosing)
  : IO[Checked[(Stamped[KeyedEvent[E]], S)]] =
    persistKeyedEvent(keyedEvent, CommitOptions.default)

  def persistKeyedEvent[E <: Event](keyedEvent: KeyedEvent[E], options: CommitOptions)
    (using enclosing: sourcecode.Enclosing)
  : IO[Checked[(Stamped[KeyedEvent[E]], S)]] =
    IO.defer:
      val E = keyedEvent.event.keyCompanion.asInstanceOf[Event.KeyCompanion[E]]
      persistEvent(using E)(key = keyedEvent.key.asInstanceOf[E.Key], options)(_ =>
        Right(keyedEvent.event))

  def persistKeyedEvents[E <: Event](
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : IO[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    persistWithOptions(options)(_ => Right(keyedEvents))

  def persistKeyedEventsLater[E <: Event](
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : IO[Checked[Unit]] =
    journaler.persist:
      Persist(
        EventCalc.pure(keyedEvents),
        options.copy(commitLater = true))
    .rightAs(())

  def persistEvent[E <: Event](using E: Event.KeyCompanion[? >: E])
    (key: E.Key, options: CommitOptions = CommitOptions.default)
    (using enclosing: sourcecode.Enclosing)
  : (S => Checked[E]) => IO[Checked[(Stamped[KeyedEvent[E]], S)]] =
    stateToEvent =>
      lock(key)(
        persistEventUnlocked(
          stateToEvent.andThen(_.map(event => key.asInstanceOf[event.keyCompanion.Key] <-: event)),
          options))

  private def persistEventUnlocked[E <: Event](
    stateToEvent: S => Checked[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : IO[Checked[(Stamped[KeyedEvent[E]], S)]] =
    persistUnlocked[E](
      EventCalc.checked: controllerState =>
        stateToEvent(controllerState).map(_ :: Nil),
      options
    ).map(_ map : (stampedKeyedEvents, state) =>
      assertThat(stampedKeyedEvents.lengthIs == 1)
      stampedKeyedEvents.head -> state)

  def persistWithOptions[E <: Event](
    options: CommitOptions = CommitOptions.default)
    (stateToEvents: S => Checked[Seq[KeyedEvent[E]]])
  : IO[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    persistUnlocked(EventCalc.checked(aggregate => stateToEvents(aggregate)), options)

  /** Persist multiple events in a transaction. */
  def persistTransaction[E <: Event](using E: Event.KeyCompanion[? >: E])(key: E.Key)
    (using enclosing: sourcecode.Enclosing)
  : (S => Checked[Seq[E]]) => IO[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    stateToEvents =>
      lock(key):
        persistUnlocked(
          EventCalc.checked: controllerState =>
            stateToEvents(controllerState)
              .map(_.map(event => key.asInstanceOf[event.keyCompanion.Key] <-: event)),
          CommitOptions(transaction = true))

  private def persistUnlocked[E <: Event](
    eventCalc: EventCalc[S, E, TimeCtx],
    options: CommitOptions)
  : IO[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    journaler.persist(Persist(eventCalc, options))
      .map(_.map: result =>
        result.stampedKeyedEvents.asInstanceOf[Seq[Stamped[KeyedEvent[E]]]] -> result.aggregate)

  def clusterState: IO[ClusterState] =
    state.map(_.clusterState)

  def unsafeCurrentState(): S =
    journaler.unsafeUncommittedAggregate()

  override def toString = s"FileJournal[$S]"


object FileJournal:

  def resource[S <: SnapshotableState[S]: Tag](
    recovered: Recovered[S],
    journalConf: JournalConf,
    eventIdGenerator: EventIdGenerator = new EventIdGenerator)
    (using
      S: SnapshotableState.Companion[S],
      ioRuntime: IORuntime,
      actorRefFactory: ActorRefFactory)
  : ResourceIO[FileJournal[S]] =
    for
      journaler <- Journaler.resource(recovered, journalConf, Some(eventIdGenerator))
      journalActor <- JournalActor.resource(journaler)
      fileJournal <- Resource.eval(IO:
        new FileJournal(journalConf, recovered.eventWatch, journaler, journalActor))
    yield
      fileJournal


  sealed trait PossibleFailover:
    private val tryingPassiveLostSwitch = Switch(false)

    // Not nestable !!! (or use a readers-writer lock)
    final def forPossibleFailoverByOtherNode[A](io: IO[A]): IO[A] =
      tryingPassiveLostSwitch.switchOnAround(io)

    final val whenNoFailoverByOtherNode: IO[Unit] =
      tryingPassiveLostSwitch.whenOff
        .logWhenItTakesLonger
