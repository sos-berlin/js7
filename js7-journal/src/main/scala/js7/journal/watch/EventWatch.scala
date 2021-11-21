package js7.journal.watch

import js7.base.problem.Checked
import js7.base.time.ScalaTime._
import js7.base.utils.CloseableIterator
import js7.base.utils.ScalaUtils.function1WithToString
import js7.data.event.{Event, EventId, EventRequest, JournalInfo, KeyedEvent, Stamped, TearableEventSeq}
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

  def observe[E <: Event](
    request: EventRequest[E],
    predicate: KeyedEvent[E] => Boolean = Every,
    onlyAcks: Boolean = false)
  : Observable[Stamped[KeyedEvent[E]]]

  def observeEventIds(timeout: Option[FiniteDuration]): Observable[EventId]

  def when[E <: Event](request: EventRequest[E], predicate: KeyedEvent[E] => Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E]]]

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

  def journalInfo: JournalInfo

  /** TEST ONLY - Blocking. */
  @TestOnly
  def await[E <: Event: ClassTag: TypeTag](
    predicate: KeyedEvent[E] => Boolean = Every,
    after: EventId = EventId.BeforeFirst,
    timeout: FiniteDuration = 99.s)
    (implicit s: Scheduler)
  : Vector[Stamped[KeyedEvent[E]]]

  @TestOnly
  def awaitAsync[E <: Event: ClassTag: TypeTag](
    predicate: KeyedEvent[E] => Boolean = Every,
    after: EventId = EventId.BeforeFirst,
    timeout: FiniteDuration = 99.s)
    (implicit s: Scheduler)
  : Task[Vector[Stamped[KeyedEvent[E]]]]

  def lastAddedEventId: EventId

  def tornEventId: EventId

  def checkEventId(eventId: EventId): Checked[Unit]
}

object EventWatch
{
  private[watch] val Every: Any => Boolean = function1WithToString("Every")(_ => true)
}
