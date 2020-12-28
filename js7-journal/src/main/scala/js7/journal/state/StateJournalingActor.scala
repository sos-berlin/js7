package js7.journal.state

import akka.actor.{ActorRef, Props}
import js7.base.problem.{Checked, Problem}
import js7.common.akkautils.SupervisorStrategies
import js7.common.scalautil.MonixUtils.promiseTask
import js7.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import js7.journal.configuration.JournalConf
import js7.journal.state.StateJournalingActor._
import js7.journal.{JournalActor, MainJournalingActor}
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Promise
import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}
import shapeless.tag.@@

private[state] final class StateJournalingActor[S <: JournaledState[S], E <: Event](
  initialState: S,
  protected val journalActor: ActorRef @@ JournalActor.type,
  protected val journalConf: JournalConf,
  persistPromise: Promise[PersistFunction[S, E]])
  (implicit S: TypeTag[S], protected val scheduler: Scheduler)
extends MainJournalingActor[S, E]
{
  override def supervisorStrategy = SupervisorStrategies.escalate

  private var state: S = initialState

  override def preStart() = {
    super.preStart()
    persistPromise.success(stateToEvent => persistStateToEvents(stateToEvent))
  }

  private def persistStateToEvents(stateToEvents: StateToEvents[S, E]): Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    promiseTask[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] { promise =>
      self ! Persist(stateToEvents, promise)
    }

  def receive = {
    case Persist(stateToEvent, promise) =>
      val tried = Try(stateToEvent(state).flatMap(keyedEvent => state.applyEvents(keyedEvent).map(_ => keyedEvent)))
      tried match {
        case Failure(t) => promise.failure(t)
        case Success(Left(problem)) => promise.success(Left(problem))
        case Success(Right(keyedEvents)) =>
          promise.completeWith(
            persistKeyedEvents(toTimestamped(keyedEvents), transaction = true, async = true) { (stampedKeyedEvents, journaledState) =>
              val updated = applyPersistedEvents(stampedKeyedEvents)
              state = updated
              Right(stampedKeyedEvents -> updated)
            })
      }
  }

  private def applyPersistedEvents(stampedKeyedEvents: Seq[Stamped[KeyedEvent[E]]]): S =
    stampedKeyedEvents.scanLeft(state)(applyPersistedEvent[S, E]).last

  override lazy val toString = s"StateJournalingActor[${S.tpe.toString.replaceAll("""^.*\.""", "")}]"

  private case class Persist(stateToEvents: StateToEvents[S, E], promise: Promise[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]])
}

private[state] object StateJournalingActor
{
  type StateToEvents[S <: JournaledState[S], E <: Event] = S => Checked[Seq[KeyedEvent[E]]]
  type PersistFunction[S <: JournaledState[S], E <: Event] = StateToEvents[S, E] => Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]]

  def props[S <: JournaledState[S], E <: Event](
    initialState: S,
    journalActor: ActorRef @@ JournalActor.type,
    journalConf: JournalConf,
    persistPromise: Promise[PersistFunction[S, E]])
    (implicit S: TypeTag[S], s: Scheduler)
  =
    Props { new StateJournalingActor(initialState, journalActor, journalConf, persistPromise) }

  private def applyPersistedEvent[S <: JournaledState[S], E <: Event] (state: S, stampedKeyedEvent: Stamped[KeyedEvent[E]]): S =
    state.withEventId(stampedKeyedEvent.eventId)
      .applyEvent(stampedKeyedEvent.value) match {
        case Left(problem) => throw new IllegalStateChangeWhilePersistingException(stampedKeyedEvent, problem)  // Serious problem !!!
        case Right(updated) => updated
      }

  private final class IllegalStateChangeWhilePersistingException(stamped: Stamped[KeyedEvent[_]], problem: Problem)
  extends RuntimeException(s"Application of event failed after persisted: $stamped: $problem")
}
