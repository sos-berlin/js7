package js7.common.event

import js7.base.utils.CloseableIterator
import js7.common.event.EventWatch.Every
import js7.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
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
final class StrictEventWatch(val underlying: EventWatch)
{
  def fileEventIds: Seq[EventId] =
    underlying.fileEventIds

  def observe[E <: Event](request: EventRequest[E], predicate: KeyedEvent[E] => Boolean = (_: KeyedEvent[E]) => true, onlyAcks: Boolean = false)
  : Observable[Stamped[KeyedEvent[E]]]
  = underlying.observe(request, predicate, onlyAcks)

  def read[E <: Event](request: EventRequest[E], predicate: KeyedEvent[E] => Boolean = Every)
  : Task[TearableEventSeq[Seq, KeyedEvent[E]]]
  = delegate(_.read(request, predicate))

  def when[E <: Event](request: EventRequest[E], predicate: KeyedEvent[E] => Boolean = Every)
  : Task[TearableEventSeq[Seq, KeyedEvent[E]]]
  = delegate(_.when(request, predicate))

  def whenAny[E <: Event](
    request: EventRequest[E],
    eventClasses: Set[Class[_ <: E]],
    predicate: KeyedEvent[E] => Boolean = Every)
  : Task[TearableEventSeq[Seq, KeyedEvent[E]]]
  = delegate(_.whenAny[E](request, eventClasses, predicate))

  def byKey[E <: Event](
    request: EventRequest[E],
    key: E#Key,
    predicate: E => Boolean = Every)
  : Task[TearableEventSeq[Seq, E]]
  = delegate(_.byKey(request, key, predicate))

  def whenKeyedEvent[E <: Event](
    request: EventRequest[E],
    key: E#Key,
    predicate: E => Boolean = Every)
  : Task[E]
  = underlying.whenKeyedEvent(request, key, predicate)

  def whenKey[E <: Event](
    request: EventRequest[E],
    key: E#Key,
    predicate: E => Boolean = Every)
  : Task[TearableEventSeq[Seq, E]]
  = delegate(_.whenKey(request, key, predicate))

  /** TEST ONLY - Blocking. */
  @TestOnly
  def await[E <: Event: ClassTag](
    predicate: KeyedEvent[E] => Boolean = Every,
    after: EventId = tornEventId,
    timeout: FiniteDuration = 99.seconds)
    (implicit s: Scheduler, E: TypeTag[E])
  : Vector[Stamped[KeyedEvent[E]]]
  = underlying.await(predicate, after, timeout)

  /** TEST ONLY - Blocking. */
  @TestOnly
  def keyedEvents[E <: Event: ClassTag: TypeTag](key: E#Key, after: EventId = tornEventId)(implicit s: Scheduler): Seq[E] =
    keyedEvents[E](after = after) collect {
      case o if o.key == key => o.event
    }

  /** TEST ONLY - Blocking. */
  @TestOnly
  def keyedEvents[E <: Event: ClassTag: TypeTag](implicit s: Scheduler): Seq[KeyedEvent[E]] =
    keyedEvents[E](after = tornEventId)

  /** TEST ONLY - Blocking. */
  @TestOnly
  def keyedEvents[E <: Event: ClassTag: TypeTag](after: EventId)(implicit s: Scheduler): Seq[KeyedEvent[E]] =
    underlying.all[E](after = after).strict match {
      case EventSeq.NonEmpty(stamped) => stamped.map(_.value)
      case EventSeq.Empty(_) => Nil
      case TearableEventSeq.Torn(eventId) => throw new TornException(after = EventId(0), tornEventId = eventId)
    }

  /** TEST ONLY - Blocking. */
  @TestOnly
  def all[E <: Event: ClassTag](implicit s: Scheduler, E: TypeTag[E]): TearableEventSeq[Seq, KeyedEvent[E]] =
    underlying.all[E]().strict

  @inline
  private def delegate[A](body: EventWatch => Task[TearableEventSeq[CloseableIterator, A]]): Task[TearableEventSeq[Seq, A]] =
    body(underlying).map(_.strict)

  def tornEventId = underlying.tornEventId

  def lastFileTornEventId = underlying.lastFileTornEventId

  def lastAddedEventId = underlying.lastAddedEventId
}
