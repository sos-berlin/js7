package js7.journal

import cats.effect.IO
import js7.base.problem.Checked
import js7.data.event.{Event, EventCalc, JournalId, JournaledState, TimeCtx}
import js7.journal.watch.EventWatch
import scala.concurrent.duration.Deadline
import scala.language.unsafeNulls

trait Journal[S <: JournaledState[S]] extends LegacyAdapter[S]:

  def journalId: JournalId

  def aggregate: IO[S]

  def unsafeAggregate(): S

  def unsafeUncommittedAggregate(): S

  def isHalted: Boolean

  def eventWatch: EventWatch

  final def persist[E <: Event](
    eventCalc: EventCalc[S, E, TimeCtx],
    commitOptions: CommitOptions = CommitOptions.default,
    since: Deadline = Deadline.now)
  : IO[Checked[Persisted[S, E]]] =
    persist(Persist(eventCalc, commitOptions, since))

  inline final def persist[E <: Event](persist: Persist[S, E]): IO[Checked[Persisted[S, E]]] =
    persist_(persist)

  protected def persist_[E <: Event](persist: Persist[S, E]): IO[Checked[Persisted[S, E]]]

  def whenNoFailoverByOtherNode: IO[Unit]
