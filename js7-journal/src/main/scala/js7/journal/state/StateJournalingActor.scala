package js7.journal.state

import akka.actor.{ActorRef, Props}
import js7.base.monixutils.MonixBase.promiseTask
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.common.akkautils.SupervisorStrategies
import js7.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import js7.journal.configuration.JournalConf
import js7.journal.state.StateJournalingActor._
import js7.journal.{CommitOptions, JournalActor, MainJournalingActor}
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Promise
import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}
import shapeless.tag.@@

private[state] final class StateJournalingActor[S <: JournaledState[S], E <: Event](
  currentState: => S,
  protected val journalActor: ActorRef @@ JournalActor.type,
  protected val journalConf: JournalConf,
  persistPromise: Promise[PersistFunction[S, E]],
  persistLaterPromise: Promise[PersistLaterFunction[E]])
  (implicit S: TypeTag[S], protected val scheduler: Scheduler)
extends MainJournalingActor[S, E]
{
  override def supervisorStrategy = SupervisorStrategies.escalate

  override def preStart() = {
    super.preStart()
    persistPromise.success((stateToEvent, options) => persistStateToEvents(stateToEvent, options))
    persistLaterPromise.success(persistLater)
  }

  private def persistStateToEvents(stateToEvents: StateToEvents[S, E], options: CommitOptions)
  : Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    promiseTask[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] { promise =>
      self ! Persist(stateToEvents, options, promise)
    }

  private def persistLater(keyedEvent: KeyedEvent[E], options: CommitOptions): Task[Checked[Unit]] =
    promiseTask[Checked[Unit]] { promise =>
      self ! PersistLater(keyedEvent, options, promise)
    }

  def receive = {
    case Persist(stateToEvents, options, promise) =>
      val state = currentState
      Try(
        for {
          keyedEvent <- stateToEvents(state)
          _ <- state.applyEvents(keyedEvent)
        } yield keyedEvent
      ) match {
        case Failure(t) => promise.failure(t)
        case Success(Left(problem)) => promise.success(Left(problem))
        case Success(Right(keyedEvents)) =>
          promise.completeWith(
            persistKeyedEvents(toTimestamped(keyedEvents), options, async = true) { (stampedKeyedEvents, journaledState) =>
              Right(stampedKeyedEvents -> journaledState)
            })
      }

    case PersistLater(keyedEvent, options, promise) =>
      promise.completeWith(
        persistKeyedEventAcceptEarlyTask(keyedEvent, options = options)
          .rightAs(())
          .runToFuture)
  }

  override lazy val toString = s"StateJournalingActor[${S.tpe.toString.replaceAll("""^.*\.""", "")}]"

  private case class Persist(
    stateToEvents: StateToEvents[S, E],
    options: CommitOptions,
    promise: Promise[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]])

  private case class PersistLater(
    keyedEvent: KeyedEvent[E],
    options: CommitOptions,
    promise: Promise[Checked[Unit]])
}

private[state] object StateJournalingActor
{
  type StateToEvents[S <: JournaledState[S], E <: Event] = S =>
    Checked[Seq[KeyedEvent[E]]]

  type PersistFunction[S <: JournaledState[S], E <: Event] =
    (StateToEvents[S, E], CommitOptions) =>
      Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]]

  type PersistLaterFunction[E <: Event] =
    (KeyedEvent[E], CommitOptions) => Task[Checked[Unit]]

  def props[S <: JournaledState[S], E <: Event](
    currentState: => S,
    journalActor: ActorRef @@ JournalActor.type,
    journalConf: JournalConf,
    persistPromise: Promise[PersistFunction[S, E]],
    persistLaterPromise: Promise[PersistLaterFunction[E]])
    (implicit S: TypeTag[S], s: Scheduler)
  =
    Props {
      new StateJournalingActor(currentState, journalActor, journalConf,
      persistPromise, persistLaterPromise)
    }

  private final class IllegalStateChangeWhilePersistingException(stamped: Stamped[KeyedEvent[_]], problem: Problem)
  extends RuntimeException(s"Application of event failed after persisted: $stamped: $problem")
}
