package js7.journal.state

import js7.base.problem.Checked
import js7.base.utils.LockKeeper
import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import js7.journal.CommitOptions
import monix.eval.Task

trait StatePersistence[S <: JournaledState[S]]
extends ReadableStatePersistence[S]
{
  protected type State = S

  def persistKeyedEvent[E <: Event](
    keyedEvent: KeyedEvent[E],
    options: CommitOptions = CommitOptions.default)
    (implicit enclosing: sourcecode.Enclosing)
  : Task[Checked[(Stamped[KeyedEvent[E]], S)]]

  final def persistKeyedEvent[E <: Event](
    keyedEvent: KeyedEvent[E],
    options: CommitOptions,
    commitLater: Boolean)
  : Task[Checked[Unit]] =
    persistKeyedEvents(keyedEvent :: Nil, options, commitLater = commitLater)

  def persistKeyedEvents[E <: Event](
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]]

  final def persistKeyedEvents[E <: Event](
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions,
    commitLater: Boolean)
  : Task[Checked[Unit]] =
    if (commitLater)
      persistKeyedEventsLater(keyedEvents, options)
    else
      persistKeyedEvents(keyedEvents, options).rightAs(())

  def persistKeyedEventsLater[E <: Event](
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : Task[Checked[Unit]]

  final def persist[E <: Event](stateToEvents: S => Checked[Seq[KeyedEvent[E]]])
  : Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    persistWithOptions(CommitOptions.default, stateToEvents)

  def persistWithOptions[E <: Event](
    options: CommitOptions = CommitOptions.default,
    stateToEvents: S => Checked[Seq[KeyedEvent[E]]])
  : Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]]

  private val keyLockKeeper = new LockKeeper[Any]

  def lock[A](key: Any)(body: Task[A])(implicit enclosing: sourcecode.Enclosing): Task[A] =
    keyLockKeeper.lock(key)(body)
}
