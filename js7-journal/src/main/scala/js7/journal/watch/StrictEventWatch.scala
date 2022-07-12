package js7.journal.watch

import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.utils.CloseableIterator
import js7.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import js7.journal.watch.EventWatch.Every
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

/**
  * Strict delegator for lazy `CloseableIterator` `EventWatch`.
  * Converts `CloseableIterator` to a `Seq` and closes the iterator.
  *
  * @author Joacim Zschimmer
  */
final class StrictEventWatch(val underlying: FileEventWatch)
{
  def fileEventIds: Seq[EventId] =
    underlying.fileEventIds

  def observe[E <: Event](
    request: EventRequest[E],
    predicate: KeyedEvent[E] => Boolean = (_: KeyedEvent[E]) => true,
    onlyAcks: Boolean = false)
  : Observable[Stamped[KeyedEvent[E]]] =
    underlying.observe(request, predicate, onlyAcks)

  def when[E <: Event](request: EventRequest[E], predicate: KeyedEvent[E] => Boolean = Every)
  : Task[TearableEventSeq[Seq, KeyedEvent[E]]] =
    delegate(_.when(request, predicate))

  def whenKeyedEvent[E <: Event](
    request: EventRequest[E],
    key: E#Key,
    predicate: E => Boolean = Every)
  : Task[E] =
    underlying.whenKeyedEvent(request, key, predicate)

  /** TEST ONLY - Blocking. */
  @TestOnly
  def await[E <: Event: ClassTag](
    predicate: KeyedEvent[E] => Boolean = Every,
    after: EventId = tornEventId,
    timeout: FiniteDuration = 99.s)
    (implicit s: Scheduler, E: TypeTag[E])
  : Vector[Stamped[KeyedEvent[E]]] =
    underlying.await(predicate, after, timeout)

  @TestOnly
  def awaitAsync[E <: Event: ClassTag: TypeTag](
    predicate: KeyedEvent[E] => Boolean = Every,
    after: EventId = EventId.BeforeFirst,
    timeout: FiniteDuration = 99.s)
    (implicit s: Scheduler)
  : Task[Vector[Stamped[KeyedEvent[E]]]] =
    underlying.awaitAsync(predicate, after, timeout)

  /** TEST ONLY - Blocking. */
  @TestOnly
  def eventsByKey[E <: Event: ClassTag: TypeTag](key: E#Key, after: EventId = tornEventId)
    (implicit s: Scheduler)
  : Seq[E] =
    keyedEvents[E](after = after)
      .collect {
        case o if o.key == key => o.event
      }

  /** TEST ONLY - Blocking. */
  @TestOnly
  def keyedEvents[E <: Event: ClassTag: TypeTag](implicit s: Scheduler): Seq[KeyedEvent[E]] =
    keyedEvents[E](after = tornEventId)

  /** TEST ONLY - Blocking. */
  @TestOnly
  def keyedEvents[E <: Event: ClassTag: TypeTag](after: EventId)(implicit s: Scheduler)
  : Seq[KeyedEvent[E]] =
    allAfter[E](after = after).await(99.s)
      .map(_.value)

  /** TEST ONLY - Blocking. */
  @TestOnly
  def allKeyedEvents[E <: Event: ClassTag](implicit s: Scheduler, E: TypeTag[E])
  : Seq[KeyedEvent[E]] =
    allStamped[E].map(_.value)

  /** TEST ONLY - Blocking. */
  @TestOnly
  def allStamped[E <: Event: ClassTag](implicit s: Scheduler, E: TypeTag[E])
  : Seq[Stamped[KeyedEvent[E]]] =
    allAfter[E]().await(99.s)

  @TestOnly
  private def allAfter[E <: Event: ClassTag: TypeTag](after: EventId = EventId.BeforeFirst)
  : Task[Seq[Stamped[KeyedEvent[E]]]] =
    when[E](EventRequest.singleClass[E](after = after), _ => true)
      .map {
        case TearableEventSeq.Torn(after) =>
          throw new TornException(after = EventId.BeforeFirst, tornEventId = after)
        case EventSeq.Empty(_) => Nil
        case EventSeq.NonEmpty(seq) => seq
      }

  @inline
  private def delegate[A](body: EventWatch => Task[TearableEventSeq[CloseableIterator, A]])
  : Task[TearableEventSeq[Seq, A]] =
    body(underlying).map(_.strict)

  def tornEventId = underlying.tornEventId

  def lastFileEventId = underlying.lastFileEventId

  def lastAddedEventId = underlying.lastAddedEventId
}
