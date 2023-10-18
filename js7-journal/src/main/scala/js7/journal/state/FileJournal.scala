package js7.journal.state

import cats.effect.Resource
import org.apache.pekko
import org.apache.pekko.actor.{ActorRef, ActorRefFactory}
import org.apache.pekko.pattern.ask
import org.apache.pekko.util.Timeout
//diffx import com.softwaremill.diffx
import com.softwaremill.tagging.{@@, Tagger}
import izumi.reflect.Tag
import js7.base.eventbus.{EventPublisher, StandardEventBus}
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.*
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
import monix.eval.Task
import monix.execution.Scheduler
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
  persistTask: Task[PersistFunction[S, Event]],
  persistLaterTask: Task[PersistLaterFunction[Event]],
  val journalActor: ActorRef @@ JournalActor.type)
  (implicit
    protected val S: SnapshotableState.Companion[S])
extends Journal[S]
with FileJournal.PossibleFailover:
  def isHalted: Boolean =
    isHaltedFun()

  def persistKeyedEvent[E <: Event](keyedEvent: KeyedEvent[E])
    (using enclosing: sourcecode.Enclosing)
  : Task[Checked[(Stamped[KeyedEvent[E]], S)]] =
    persistKeyedEvent(keyedEvent, CommitOptions.default)

  def persistKeyedEvent[E <: Event](keyedEvent: KeyedEvent[E], options: CommitOptions)
    (using enclosing: sourcecode.Enclosing)
  : Task[Checked[(Stamped[KeyedEvent[E]], S)]] =
    Task.defer:
      val E = keyedEvent.event.keyCompanion.asInstanceOf[Event.KeyCompanion[E]]
      persistEvent(using E)(key = keyedEvent.key.asInstanceOf[E.Key], options)(_ =>
        Right(keyedEvent.event))

  def persistKeyedEvents[E <: Event](
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    persistWithOptions(options)(_ => Right(keyedEvents))

  def persistKeyedEventsLater[E <: Event](
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : Task[Checked[Unit]] =
    persistLaterTask.flatMap(_(keyedEvents, options))

  def persistEvent[E <: Event](using E: Event.KeyCompanion[? >: E])
    (key: E.Key, options: CommitOptions = CommitOptions.default)
    (using enclosing: sourcecode.Enclosing)
  : (S => Checked[E]) => Task[Checked[(Stamped[KeyedEvent[E]], S)]] =
    stateToEvent =>
      lock(key)(
        persistEventUnlocked(
          stateToEvent.andThen(_.map(event => key.asInstanceOf[event.keyCompanion.Key] <-: event)),
          options))

  private def persistEventUnlocked[E <: Event](
    stateToEvent: S => Checked[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : Task[Checked[(Stamped[KeyedEvent[E]], S)]] =
    persistTask
      .flatMap(_(state => stateToEvent(state).map(_ :: Nil), options, CorrelId.current))
      .map(_ map {
        case (stampedKeyedEvents, state) =>
          assertThat(stampedKeyedEvents.lengthIs == 1)
          stampedKeyedEvents.head.asInstanceOf[Stamped[KeyedEvent[E]]] -> state
      })

  def persistWithOptions[E <: Event](
    options: CommitOptions = CommitOptions.default)
    (stateToEvents: S => Checked[Seq[KeyedEvent[E]]])
  : Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    Task.defer:
      persistUnlocked(stateToEvents, options)

  /** Persist multiple events in a transaction. */
  def persistTransaction[E <: Event](using E: Event.KeyCompanion[? >: E])(key: E.Key)
  : (S => Checked[Seq[E]]) => Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    stateToEvents => Task.defer:
      lock(key)(
        persistUnlocked(
          state => stateToEvents(state)
            .map(_.map(event => key.asInstanceOf[event.keyCompanion.Key] <-: event)),
          CommitOptions(transaction = true)))

  private def persistUnlocked[E <: Event](
    stateToEvents: StateToEvents[S, E],
    options: CommitOptions)
  : Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    Task.defer:
      persistTask.flatMap(
        _(stateToEvents, options, CorrelId.current)
          .map(_.map { case (stampedKeyedEvents, state) =>
            stampedKeyedEvents.asInstanceOf[Seq[Stamped[KeyedEvent[E]]]] -> state }))

  def clusterState: Task[ClusterState] =
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
      scheduler: Scheduler,
      actorRefFactory: ActorRefFactory,
      timeout: pekko.util.Timeout)
  : Resource[Task, FileJournal[S]] =
    Resource.make(
      acquire = Task.defer {
        import recovered.journalLocation
        val S = implicitly[SnapshotableState.Companion[S]]
        val journalId = recovered.journalId getOrElse JournalId.random()

        if recovered.recoveredJournalFile.isEmpty then {
          logger.info("Starting a new empty journal")
        }

        val journalActorStopped = Promise[JournalActor.Stopped]()
        val journalActor = actorRefFactory
          .actorOf(
            JournalActor.props[S](journalLocation, journalConf, keyedEventBus, scheduler,
              eventIdGenerator, journalActorStopped),
            "Journal")
          .taggedWith[JournalActor.type]


        val whenJournalActorReady = Task.fromFuture(
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
              getState <- Task
                .deferFuture((journalActor ? JournalActor.Input.GetJournaledState).mapTo[() => S])
                .logWhenItTakesLonger("JournalActor.Input.GetJournaledState")
              isHalted <- Task
                .deferFuture((journalActor ? JournalActor.Input.GetIsHaltedFunction).mapTo[() =>
                  Boolean])
                .logWhenItTakesLonger("JournalActor.Input.GetIsHaltedFunction")
            yield (journalHeader, getState, isHalted)
          }
          .flatMap { case (journalHeader, getCurrentState, isHalted) =>
            Task {
              val persistPromise = Promise[PersistFunction[S, Event]]()
              val persistTask = Task.fromFuture(persistPromise.future)
              val persistLaterPromise = Promise[PersistLaterFunction[Event]]()
              val persistLaterTask = Task.fromFuture(persistLaterPromise.future)

              val actor = actorRefFactory
                .actorOf(
                  StateJournalingActor.props[S, Event](
                    getCurrentState, journalActor, journalConf, persistPromise, persistLaterPromise),
                  encodeAsActorName("StateJournalingActor:" + S))
                .taggedWith[StateJournalingActor.type]

              val journal = new FileJournal[S](
                journalId, journalHeader, journalConf,
                recovered.eventWatch,
                getCurrentState, isHalted, persistTask, persistLaterTask,
                journalActor)

              (journal, journalActor, actor, journalActorStopped)
            }
          }
      })(
      release = { case (journal, journalActor, actor, journalActorStopped) =>
        logger.debugTask(s"$journal stop")(
          Task.defer {
            actorRefFactory.stop(actor)
            journalActor ! JournalActor.Input.Terminate
            Task.fromFuture(journalActorStopped.future).void
              .logWhenItTakesLonger("JournalActor.Input.Terminate")
          })
      })
      .map(_._1)

  sealed trait PossibleFailover:
    private val tryingPassiveLostSwitch = Switch(false)

    // Not nestable !!! (or use a readers-writer lock)
    final def forPossibleFailoverByOtherNode[A](task: Task[A]): Task[A] =
      tryingPassiveLostSwitch.switchOnFor(task)

    final val whenNoFailoverByOtherNode: Task[Unit] =
      tryingPassiveLostSwitch.whenOff
