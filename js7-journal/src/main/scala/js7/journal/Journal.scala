package js7.journal

import cats.effect.IO
import js7.base.problem.Checked
import js7.base.service.Service
import js7.data.event.{Event, EventCalc, JournalId, JournaledState, KeyedEvent, Stamped, TimeCtx}
import js7.journal.CommitOptions.Transaction
import js7.journal.watch.EventWatch
import scala.concurrent.duration.Deadline
import scala.language.unsafeNulls

trait Journal[S <: JournaledState[S]] extends Service:

  def journalId: JournalId

  def aggregate: IO[S]

  def unsafeAggregate(): S

  def unsafeUncommittedAggregate(): S

  def isHalted: Boolean

  def eventWatch: EventWatch

  protected def persist_[E <: Event](persist: Persist[S, E]): IO[Checked[Persisted[S, E]]]

  def whenNoFailoverByOtherNode: IO[Unit]

  inline final def persist[E <: Event](keyedEvent: KeyedEvent[E])
    (using enclosing: sourcecode.Enclosing)
  : IO[Checked[(Stamped[KeyedEvent[E]], S)]] =
    persistKeyedEvent(keyedEvent)

  inline final def persist[E <: Event](keyedEvents: IterableOnce[KeyedEvent[E]])
  : IO[Checked[Persisted[S, E]]] =
    persistKeyedEvents(keyedEvents)

  final def persist[E <: Event](keyedEvent: KeyedEvent[E]): IO[Checked[Persisted[S, E]]] =
    persist[E]():
      EventCalc.pure(keyedEvent)

  final def persist[E <: Event](
    commitOptions: CommitOptions = CommitOptions.default,
    since: Deadline = Deadline.now)
    (eventCalc: EventCalc[S, E, TimeCtx])
  : IO[Checked[Persisted[S, E]]] =
    persist_(Persist(commitOptions, since)(eventCalc))

  final def persist[E <: Event](aggregateToEvents: S => Checked[IterableOnce[KeyedEvent[E]]])
  : IO[Checked[Persisted[S, E]]] =
    persistChecked()(aggregateToEvents)

  final def persist[E <: Event](eventCalc: EventCalc[S, E, TimeCtx]): IO[Checked[Persisted[S, E]]] =
    persist_(Persist(eventCalc))

  inline final def persist[E <: Event](persist: Persist[S, E]): IO[Checked[Persisted[S, E]]] =
    persist_(persist)

  final def persistKeyedEvent[E <: Event](keyedEvent: KeyedEvent[E])
    (using enclosing: sourcecode.Enclosing)
  : IO[Checked[(Stamped[KeyedEvent[E]], S)]] =
    persist[E]():
      EventCalc.pure(keyedEvent)
    .map(_ flatMap: persisted =>
      persisted.checkedSingle)

  final def persistKeyedEvents[E <: Event](keyedEvents: IterableOnce[KeyedEvent[E]])
  : IO[Checked[Persisted[S, E]]] =
    persistKeyedEvents()(keyedEvents)

  final def persistKeyedEvents[E <: Event](
    options: CommitOptions = CommitOptions.default)
    (keyedEvents: IterableOnce[KeyedEvent[E]])
  : IO[Checked[Persisted[S, E]]] =
    persist(options):
      EventCalc.pure(keyedEvents)

  final def persistTransaction[E <: Event](aggregateToEvents: S => Checked[IterableOnce[KeyedEvent[E]]])
  : IO[Checked[Persisted[S, E]]] =
    persistChecked[E](Transaction)(aggregateToEvents)

  final def persistChecked[E <: Event](
    options: CommitOptions = CommitOptions.default)
    (aggregateToEvents: S => Checked[IterableOnce[KeyedEvent[E]]])
  : IO[Checked[Persisted[S, E]]] =
    persist[E](options):
      EventCalc.checked: controllerState =>
        aggregateToEvents(controllerState)
