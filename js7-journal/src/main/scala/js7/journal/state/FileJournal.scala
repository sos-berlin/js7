package js7.journal.state

import cats.effect.{Resource, ResourceIO}
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import org.apache.pekko
import org.apache.pekko.actor.{ActorRef, ActorRefFactory}
import org.apache.pekko.pattern.ask
import org.apache.pekko.util.Timeout
import scala.concurrent.ExecutionContext
//diffx import com.softwaremill.diffx
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.softwaremill.tagging.{@@, Tagger}
import izumi.reflect.Tag
import js7.base.eventbus.{EventPublisher, StandardEventBus}
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.Switch
import js7.base.problem.Checked
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.common.pekkoutils.Pekkos.encodeAsActorName
import js7.data.cluster.ClusterState
import js7.data.event.{AnyKeyedEvent, Event, JournalHeader, JournalHeaders, JournalId, KeyedEvent, SnapshotableState, Stamped}
import js7.journal.configuration.JournalConf
import js7.journal.recover.Recovered
import js7.journal.state.StateJournalingActor.{PersistFunction, PersistLaterFunction, StateToEvents}
import js7.journal.watch.FileEventWatch
import js7.journal.{CommitOptions, EventIdGenerator, JournalActor}
import scala.concurrent.Promise
import sourcecode.Enclosing

// TODO Lock for NoKey is to wide. Restrict to a set of Event superclasses, like ClusterEvent, ControllerEvent?
//  Der Aufrufer kann sich um die Sperren uns dessen Granularität kümmern.
//  FileJournal stellt dazu LockKeeper bereit
//  Wir werden vielleicht mehrere Schlüssel auf einmal sperren wollen (für fork/join?)

final class FileJournal[S <: SnapshotableState[S]: Tag] private(
  val journalId: JournalId,
  val journalHeader: JournalHeader,
  val journalConf: JournalConf,
  val eventWatch: FileEventWatch,
  getCurrentState: () => S,
  isHaltedFun: () => Boolean,
  persistIO: IO[PersistFunction[S, Event]],
  persistLaterIO: IO[PersistLaterFunction[Event]],
  val journalActor: ActorRef @@ JournalActor.type)
  (implicit
    protected val S: SnapshotableState.Companion[S])
extends Journal[S], FileJournal.PossibleFailover:

  def isHalted: Boolean =
    isHaltedFun()

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
    persistLaterIO.flatMap(_(keyedEvents, options))

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
    persistIO
      .flatMap(_(state => stateToEvent(state).map(_ :: Nil), options, CorrelId.current))
      .map(_ map {
        case (stampedKeyedEvents, state) =>
          assertThat(stampedKeyedEvents.lengthIs == 1)
          stampedKeyedEvents.head.asInstanceOf[Stamped[KeyedEvent[E]]] -> state
      })

  def persistWithOptions[E <: Event](
    options: CommitOptions = CommitOptions.default)
    (stateToEvents: S => Checked[Seq[KeyedEvent[E]]])
  : IO[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    IO.defer:
      persistUnlocked(stateToEvents, options)

  /** Persist multiple events in a transaction. */
  def persistTransaction[E <: Event](using E: Event.KeyCompanion[? >: E])(key: E.Key)
  : (S => Checked[Seq[E]]) => IO[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    stateToEvents => IO.defer:
      lock(key)(
        persistUnlocked(
          state => stateToEvents(state)
            .map(_.map(event => key.asInstanceOf[event.keyCompanion.Key] <-: event)),
          CommitOptions(transaction = true)))

  private def persistUnlocked[E <: Event](
    stateToEvents: StateToEvents[S, E],
    options: CommitOptions)
  : IO[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    IO.defer:
      persistIO.flatMap(
        _(stateToEvents, options, CorrelId.current)
          .map(_.map { case (stampedKeyedEvents, state) =>
            stampedKeyedEvents.asInstanceOf[Seq[Stamped[KeyedEvent[E]]]] -> state }))

  def clusterState: IO[ClusterState] =
    state.map(_.clusterState)

  def unsafeCurrentState(): S =
    getCurrentState()

  override def toString = s"FileJournal[$S]"


object FileJournal:
  private val logger = Logger[this.type]

  def resource[S <: SnapshotableState[S]: SnapshotableState.Companion/*: diffx.Diff*/: Tag](
    recovered: Recovered[S],
    journalConf: JournalConf,
    eventIdGenerator: EventIdGenerator = new EventIdGenerator,
    keyedEventBus: EventPublisher[Stamped[AnyKeyedEvent]] = new StandardEventBus)
    (implicit
      ioRuntime: IORuntime,
      actorRefFactory: ActorRefFactory,
      timeout: pekko.util.Timeout)
  : ResourceIO[FileJournal[S]] =
    Resource.make(
      acquire = IO.defer {
        import recovered.journalLocation
        given ExecutionContext = ioRuntime.compute

        val S = implicitly[SnapshotableState.Companion[S]]
        val journalId = recovered.journalId getOrElse JournalId.random()

        if recovered.recoveredJournalFile.isEmpty then
          logger.info("Starting a new empty journal")

        val journalActorStopped = Promise[JournalActor.Stopped]()
        val journalActor = actorRefFactory
          .actorOf(
            JournalActor.props[S](journalLocation, journalConf, keyedEventBus, ioRuntime,
              eventIdGenerator, journalActorStopped),
            "Journal")
          .taggedWith[JournalActor.type]


        val whenJournalActorReady = IO.fromFuture(IO:
          (journalActor ?
            JournalActor.Input.Start(
              recovered.state,
              Some(recovered.eventWatch),
              recovered.recoveredJournalFile.fold(JournalHeaders.initial[S](journalId))(_.nextJournalHeader),
              recovered.totalRunningSince)
            )(Timeout(1.h /*???*/)
          ).mapTo[JournalActor.Output.Ready].map(_.journalHeader))

        whenJournalActorReady
          .flatMap { journalHeader =>
            logger.debug("JournalActor is ready")
            for
              getState <- IO
                .fromFuture(IO:
                  (journalActor ? JournalActor.Input.GetJournaledState).mapTo[() => S])
                .logWhenItTakesLonger("JournalActor.Input.GetJournaledState")
              isHalted <-
                IO.fromFuture(IO:
                    (journalActor ? JournalActor.Input.GetIsHaltedFunction).mapTo[() => Boolean])
                  .logWhenItTakesLonger("JournalActor.Input.GetIsHaltedFunction")
            yield (journalHeader, getState, isHalted)
          }
          .flatMap { case (journalHeader, getCurrentState, isHalted) =>
            IO {
              val persistPromise = Promise[PersistFunction[S, Event]]()
              val persistIO = IO.fromFuture(IO.pure(persistPromise.future))
              val persistLaterPromise = Promise[PersistLaterFunction[Event]]()
              val persistLaterIO = IO.fromFuture(IO.pure(persistLaterPromise.future))

              val actor = actorRefFactory
                .actorOf(
                  StateJournalingActor.props[S, Event](
                    getCurrentState, journalActor, journalConf, persistPromise, persistLaterPromise),
                  encodeAsActorName("StateJournalingActor:" + S))
                .taggedWith[StateJournalingActor.type]

              val journal = new FileJournal[S](
                journalId, journalHeader, journalConf,
                recovered.eventWatch,
                getCurrentState, isHalted, persistIO, persistLaterIO,
                journalActor)

              (journal, journalActor, actor, journalActorStopped)
            }
          }
      })(
      release = { case (journal, journalActor, actor, journalActorStopped) =>
        logger.debugIO(s"$journal stop")(
          IO.defer {
            actorRefFactory.stop(actor)
            journalActor ! JournalActor.Input.Terminate
            IO.fromFuture(IO.pure(journalActorStopped.future)).void
              .logWhenItTakesLonger("JournalActor.Input.Terminate")
          })
      })
      .map(_._1)

  sealed trait PossibleFailover:
    private val tryingPassiveLostSwitch = Switch(false)

    // Not nestable !!! (or use a readers-writer lock)
    final def forPossibleFailoverByOtherNode[A](io: IO[A]): IO[A] =
      tryingPassiveLostSwitch.switchOnAround(io)

    final val whenNoFailoverByOtherNode: IO[Unit] =
      tryingPassiveLostSwitch.whenOff
        .logWhenItTakesLonger
