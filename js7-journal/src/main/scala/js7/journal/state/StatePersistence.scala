package js7.journal.state

import js7.base.problem.Checked
import js7.base.utils.LockKeeper
import js7.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import js7.journal.watch.EventWatch
import monix.eval.Task

trait StatePersistence[S <: JournaledState[S]]
{
  protected val S: JournaledState.Companion[S]

  def currentState: S

  final def awaitCurrentState: Task[S] =
    waitUntilStarted >> Task(currentState)

  def waitUntilStarted: Task[Unit]

  def eventWatch: EventWatch

  def persistKeyedEvent[E <: Event](keyedEvent: KeyedEvent[E])
  : Task[Checked[(Stamped[KeyedEvent[E]], S)]]

  def persist[E <: Event](stateToEvents: S => Checked[Seq[KeyedEvent[E]]])
  : Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]]

  private val keyLockKeeper = new LockKeeper[Any]

  def lock[A](key: Any)(body: Task[A]): Task[A] =
    keyLockKeeper.lock(key)(body)
}
