package js7.journal.watch

import js7.base.data.ByteArray
import js7.base.problem.Checked
import js7.base.time.ScalaTime._
import js7.base.utils.CloseableIterator
import js7.base.utils.ScalaUtils.function1WithToString
import js7.common.jsonseq.PositionAnd
import js7.data.event.{Event, EventId, EventRequest, JournalInfo, JournalPosition, KeyedEvent, Stamped, TearableEventSeq}
import js7.journal.watch.EventWatch._
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

/**
  * @author Joacim Zschimmer
  */
trait EventWatch
{
  def started: Task[this.type] =
    Task.fromFuture(whenStarted).memoize.asInstanceOf[Task[this.type]]

  def whenStarted: Future[this.type] = Future.successful(this)

  def strict: StrictEventWatch = new StrictEventWatch(this)

  def observe[E <: Event](request: EventRequest[E], predicate: KeyedEvent[E] => Boolean = Every, onlyAcks: Boolean = false)
  : Observable[Stamped[KeyedEvent[E]]]

  def observeEventIds(timeout: Option[FiniteDuration]): Observable[EventId]

  def read[E <: Event](request: EventRequest[E], predicate: KeyedEvent[E] => Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E]]]

  def when[E <: Event](request: EventRequest[E], predicate: KeyedEvent[E] => Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E]]]

  def whenAny[E <: Event](
    request: EventRequest[E],
    eventClasses: Set[Class[_ <: E]],
    predicate: KeyedEvent[E] => Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E]]]

  def byKey[E <: Event](
    request: EventRequest[E],
    key: E#Key,
    predicate: E => Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, E]]

  def whenKeyedEvent[E <: Event](
    request: EventRequest[E],
    key: E#Key,
    predicate: E => Boolean = Every)
  : Task[E]

  def whenKey[E <: Event](
    request: EventRequest[E],
    key: E#Key,
    predicate: E => Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, E]]

  /** Returns None as last element iff timeout has elapsed.
    * @param journalPosition None to use defaults for manual request of the current journal file stream,
    *                        just to show something.
    */
  def observeFile(journalPosition: JournalPosition,
    timeout: FiniteDuration, markEOF: Boolean = false, onlyAcks: Boolean = false)
  : Task[Checked[Observable[PositionAnd[ByteArray]]]]

  def snapshotAfter(after: EventId): Option[Observable[Any]]

  def rawSnapshotAfter(after: EventId): Option[Observable[ByteArray]]

  def journalPosition: Checked[JournalPosition]

  def journalInfo: JournalInfo

  /** TEST ONLY - Blocking. */
  @TestOnly
  def await[E <: Event: ClassTag: TypeTag](
    predicate: KeyedEvent[E] => Boolean = Every,
    after: EventId = EventId.BeforeFirst,
    timeout: FiniteDuration = 99.s)
    (implicit s: Scheduler)
  : Vector[Stamped[KeyedEvent[E]]]

  /** TEST ONLY - Blocking. */
  @TestOnly
  def all[E <: Event: ClassTag: TypeTag](after: EventId = tornEventId)(implicit s: Scheduler): TearableEventSeq[CloseableIterator, KeyedEvent[E]]

  def fileEventIds: Seq[EventId]

  final def tornEventId = fileEventIds.headOption getOrElse EventId.BeforeFirst

  final def lastFileTornEventId = fileEventIds.last

  def lastAddedEventId: EventId
}

object EventWatch
{
  private[watch] val Every: Any => Boolean = function1WithToString("Every")(_ => true)
}
