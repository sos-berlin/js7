package js7.journal.state

import js7.base.problem.Checked
import js7.base.utils.LockKeeper
import js7.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import js7.journal.CommitOptions
import js7.journal.watch.EventWatch
import monix.eval.Task

trait AnyStatePersistence
{
  protected type State <: JournaledState[State]

  protected val S: JournaledState.Companion[State]

  def currentState: State

  final def awaitCurrentState: Task[State] =
    waitUntilStarted >> Task(currentState)

  def waitUntilStarted: Task[Unit]

  def eventWatch: EventWatch

  def persistKeyedEvent[E <: Event](
    keyedEvent: KeyedEvent[E],
    options: CommitOptions = CommitOptions.default)
  : Task[Checked[(Stamped[KeyedEvent[E]], State)]]

  def persistKeyedEventLater[E <: Event](
    keyedEvent: KeyedEvent[E],
    options: CommitOptions = CommitOptions.default)
  : Task[Checked[Unit]]

  def persist[E <: Event](stateToEvents: State => Checked[Seq[KeyedEvent[E]]])
  : Task[Checked[(Seq[Stamped[KeyedEvent[E]]], State)]]

  private val keyLockKeeper = new LockKeeper[Any]

  def lock[A](key: Any)(body: Task[A]): Task[A] =
    keyLockKeeper.lock(key)(body)
}
