package js7.journal.state

import cats.effect.IO
import js7.base.problem.Checked
import js7.base.utils.LockKeeper
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.{Event, JournalId, JournaledState, KeyedEvent, Stamped}
import js7.journal.CommitOptions

trait Journal[S <: JournaledState[S]]
extends ReadableStateJournal[S]:
  protected type State = S

  def journalId: JournalId

  def isHalted: Boolean

  def persistKeyedEvent[E <: Event](
    keyedEvent: KeyedEvent[E],
    options: CommitOptions = CommitOptions.default)
    (using enclosing: sourcecode.Enclosing)
  : IO[Checked[(Stamped[KeyedEvent[E]], S)]]

  final def persistKeyedEvent[E <: Event](
    keyedEvent: KeyedEvent[E],
    options: CommitOptions,
    commitLater: Boolean)
  : IO[Checked[Unit]] =
    persistKeyedEvents(keyedEvent :: Nil, options, commitLater = commitLater)

  def persistKeyedEvents[E <: Event](
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : IO[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]]

  final def persistKeyedEvents[E <: Event](
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions,
    commitLater: Boolean)
  : IO[Checked[Unit]] =
    if commitLater then
      persistKeyedEventsLater(keyedEvents, options)
    else
      persistKeyedEvents(keyedEvents, options).rightAs(())

  def persistKeyedEventsLater[E <: Event](
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : IO[Checked[Unit]]

  final def persist1[E <: Event](stateToEvents: S => Checked[KeyedEvent[E]])
  : IO[Checked[(Stamped[KeyedEvent[E]], S)]] =
    persist: state =>
      stateToEvents(state).map(_ :: Nil)
    .map(_.map:
      case (Seq(stampedEvent), updated) => stampedEvent -> updated)

  final def persist[E <: Event](stateToEvents: S => Checked[Seq[KeyedEvent[E]]])
  : IO[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    persistWithOptions(CommitOptions.default)(stateToEvents)

  def persistWithOptions[E <: Event](
    options: CommitOptions = CommitOptions.default)
    (stateToEvents: S => Checked[Seq[KeyedEvent[E]]])
  : IO[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]]

  private val keyLockKeeper = new LockKeeper[Any]

  def lock[A](key: Any)(body: IO[A])(implicit enclosing: sourcecode.Enclosing): IO[A] =
    keyLockKeeper.lock(key)(body)

  val whenNoFailoverByOtherNode: IO[Unit]
