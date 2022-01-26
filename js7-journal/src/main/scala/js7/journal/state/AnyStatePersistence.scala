package js7.journal.state

import js7.base.problem.Checked
import js7.base.utils.LockKeeper
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import js7.journal.CommitOptions
import js7.journal.watch.EventWatch
import monix.eval.Task

trait AnyStatePersistence
{
  protected type State <: JournaledState[State]
  private type S = State

  protected val S: JournaledState.Companion[S]

  def currentState: S

  final val state: Task[S] =
    Task(currentState)

  final def awaitCurrentState: Task[S] =
    waitUntilStarted >> Task(currentState)

  def waitUntilStarted: Task[Unit]

  def eventWatch: EventWatch

  def persistKeyedEvent[E <: Event](
    keyedEvent: KeyedEvent[E],
    options: CommitOptions = CommitOptions.default)
  : Task[Checked[(Stamped[KeyedEvent[E]], S)]]

  final def persistKeyedEvent[E <: Event](
    keyedEvent: KeyedEvent[E],
    options: CommitOptions,
    commitLater: Boolean)
  : Task[Checked[Unit]] =
    if (commitLater)
      persistKeyedEventLater(keyedEvent, options)
    else
      persistKeyedEvent(keyedEvent, options).rightAs(())

  def persistKeyedEvents[E <: Event](
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]]

  def persistKeyedEventLater[E <: Event](
    keyedEvent: KeyedEvent[E],
    options: CommitOptions = CommitOptions.default)
  : Task[Checked[Unit]]

  def persist[E <: Event](stateToEvents: S => Checked[Seq[KeyedEvent[E]]])
  : Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]]

  private val keyLockKeeper = new LockKeeper[Any]

  def lock[A](key: Any)(body: Task[A]): Task[A] =
    keyLockKeeper.lock(key)(body)
}
