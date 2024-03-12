package js7.journal.state

import org.apache.pekko.actor.{ActorRef, Props}
import com.softwaremill.tagging.@@
import izumi.reflect.Tag
import js7.base.log.CorrelId
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.common.pekkoutils.SupervisorStrategies
import js7.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import js7.journal.configuration.JournalConf
import js7.journal.state.StateJournalingActor.*
import js7.journal.{CommitOptions, JournalActor, MainJournalingActor}
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.base.catsutils.CatsEffectUtils
import js7.base.catsutils.CatsEffectUtils.promiseIO
import scala.concurrent.Promise
import scala.util.{Failure, Success, Try}

private[state] final class StateJournalingActor[S <: JournaledState[S], E <: Event](
  currentState: () => S,
  protected val journalActor: ActorRef @@ JournalActor.type,
  protected val journalConf: JournalConf,
  persistPromise: Promise[PersistFunction[S, E]],
  persistLaterPromise: Promise[PersistLaterFunction[E]])
  (implicit S: Tag[S], protected val ioRuntime: IORuntime)
extends MainJournalingActor[S, E]:

  override def supervisorStrategy = SupervisorStrategies.escalate

  override def preStart() =
    super.preStart()
    persistPromise.success((stateToEvent, options, correlId) =>
      persistStateToEvents(stateToEvent, options, correlId))
    persistLaterPromise.success(persistLater)

  private def persistStateToEvents(
    stateToEvents: StateToEvents[S, E],
    options: CommitOptions,
    correlId: CorrelId)
  : IO[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    promiseIO[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] { promise =>
      self ! Persist(stateToEvents, options, correlId, promise)
    }

  private def persistLater(keyedEvents: Seq[KeyedEvent[E]], options: CommitOptions): IO[Checked[Unit]] =
    promiseIO[Checked[Unit]] { promise =>
      self ! PersistLater(keyedEvents, options, promise)
    }

  def receive =
    case Persist(stateToEvents, options, correlId, promise) =>
      correlId.bind[Unit]:
        val state = currentState()
        Try(
          for
            keyedEvent <- stateToEvents(state)
            _ <- state.applyEvents(keyedEvent)
          yield keyedEvent
        ) match
          case Failure(t) => promise.failure(t)
          case Success(Left(problem)) => promise.success(Left(problem))
          case Success(Right(keyedEvents)) =>
            promise.completeWith(
              persistKeyedEventsReturnChecked(toTimestamped(keyedEvents), options, async = true) {
                (stampedKeyedEvents, journaledState) =>
                  stampedKeyedEvents -> journaledState
              })

    case PersistLater(keyedEvents, options, promise) =>
      promise.completeWith(
        persistKeyedEventAcceptEarlyIO(keyedEvents, options = options)
          .rightAs(())
          .unsafeToFuture())

  override lazy val toString = s"StateJournalingActor[${S.tag.toString.replaceAll("""^.*\.""", "")}]"

  private case class Persist(
    stateToEvents: StateToEvents[S, E],
    options: CommitOptions,
    correlId: CorrelId,
    promise: Promise[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]])

  private case class PersistLater(
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions,
    promise: Promise[Checked[Unit]])

private[state] object StateJournalingActor:
  type StateToEvents[S <: JournaledState[S], E <: Event] = S =>
    Checked[Seq[KeyedEvent[E]]]

  type PersistFunction[S <: JournaledState[S], E <: Event] =
    (StateToEvents[S, E], CommitOptions, CorrelId) =>
      IO[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]]

  type PersistLaterFunction[E <: Event] =
    (Seq[KeyedEvent[E]], CommitOptions) => IO[Checked[Unit]]

  def props[S <: JournaledState[S], E <: Event](
    currentState: () => S,
    journalActor: ActorRef @@ JournalActor.type,
    journalConf: JournalConf,
    persistPromise: Promise[PersistFunction[S, E]],
    persistLaterPromise: Promise[PersistLaterFunction[E]])
    (implicit S: Tag[S], ioRutime: IORuntime)
  =
    Props:
      new StateJournalingActor(currentState, journalActor, journalConf,
      persistPromise, persistLaterPromise)

  //private final class IllegalStateChangeWhilePersistingException(stamped: Stamped[KeyedEvent[?]], problem: Problem)
  //extends RuntimeException(s"Application of event failed after persisted: $stamped: $problem")
