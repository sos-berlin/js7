package js7.journal.watch

import izumi.reflect.Tag
import js7.base.problem.Checked
import js7.base.time.ScalaTime.*
import js7.base.utils.CloseableIterator
import js7.base.utils.ScalaUtils.function1WithToString
import js7.data.event.{Event, EventId, EventRequest, JournalInfo, KeyedEvent, Stamped, TearableEventSeq}
import js7.journal.watch.EventWatch.*
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
trait EventWatch:
  def started: Task[this.type] =
    Task.fromFuture(whenStarted).memoize.asInstanceOf[Task[this.type]]

  def whenStarted: Future[this.type] = Future.successful(this)

  def observe[E <: Event](
    request: EventRequest[E],
    predicate: KeyedEvent[E] => Boolean = Every,
    onlyAcks: Boolean = false)
  : Observable[Stamped[KeyedEvent[E]]]

  def observeEventIds(timeout: Option[FiniteDuration]): Task[Checked[Observable[EventId]]]

  def when[E <: Event](request: EventRequest[E], predicate: KeyedEvent[E] => Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E]]]

  def whenKeyedEvent[E <: Event](using E: Event.KeyCompanion[? >: E])(
    request: EventRequest[E],
    key: E.Key,
    predicate: E => Boolean = Every)
  : Task[E]

  def whenKey[E <: Event](using E: Event.KeyCompanion[? >: E])
    (request: EventRequest[E],
    key: E.Key,
    predicate: E => Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, E]]

  def journalInfo: JournalInfo

  def untilAllKeys[E <: Event : ClassTag](using E: Event.KeyCompanion[? >: E])(
    keys: IterableOnce[E.Key],
    predicate: KeyedEvent[E] => Boolean = Every,
    after: EventId,
    timeout: Option[FiniteDuration])
  : Task[Seq[Stamped[KeyedEvent[E]]]] =
    observe(EventRequest.singleClass[E](after = after, timeout = timeout))
      .filter(stamped => predicate(stamped.value))
      .mapAccumulate(Set.from(keys)) { (keys, stamped) =>
        val minishedKeys = keys - stamped.value.key.asInstanceOf[E.Key]
        (minishedKeys, (stamped, minishedKeys.nonEmpty))
      }
      .takeWhile(_._2)
      .map(_._1)
      .toListL

  /** TEST ONLY - Blocking. */
  @TestOnly
  def await[E <: Event: ClassTag: Tag](
    predicate: KeyedEvent[E] => Boolean = Every,
    after: EventId = EventId.BeforeFirst,
    timeout: FiniteDuration = 99.s)
    (implicit s: Scheduler)
  : Vector[Stamped[KeyedEvent[E]]]

  @TestOnly
  def awaitAsync[E <: Event: ClassTag](
    predicate: KeyedEvent[E] => Boolean = Every,
    after: EventId = EventId.BeforeFirst,
    timeout: FiniteDuration = 99.s)
    (implicit s: Scheduler)
  : Task[Vector[Stamped[KeyedEvent[E]]]]

  def lastAddedEventId: EventId

  def tornEventId: EventId

  def checkEventId(eventId: EventId): Checked[Unit]

object EventWatch:
  private[watch] val Every: Any => Boolean = function1WithToString("Every")(_ => true)
