package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.common.event.EventWatch.Every
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

/**
  * Strict delegator for lazy `CloseableIterator` `EventWatch`.
  * Converts `CloseableIterator` to a `Seq` and closes the iterator.
  *
  * @author Joacim Zschimmer
  */
final class StrictEventWatch(eventWatch: EventWatch)
{
  def observe[E <: Event](request: EventRequest[E], predicate: KeyedEvent[E] => Boolean = (_: KeyedEvent[E]) => true, onlyLastOfChunk: Boolean)
  : Observable[Stamped[KeyedEvent[E]]]
  = eventWatch.observe(request, predicate, onlyLastOfChunk)

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
  = eventWatch.whenKeyedEvent(request, key, predicate)

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
    after: EventId = EventId.BeforeFirst,
    timeout: FiniteDuration = 99.seconds)
    (implicit s: Scheduler, E: TypeTag[E])
  : Vector[Stamped[KeyedEvent[E]]]
  = eventWatch.await(predicate, after, timeout)

  /** TEST ONLY - Blocking. */
  @TestOnly
  def keyedEvents[E <: Event: ClassTag: TypeTag](key: E#Key)(implicit s: Scheduler): Seq[E] =
    keyedEvents[E] collect {
      case o if o.key == key => o.event
    }

  /** TEST ONLY - Blocking. */
  @TestOnly
  def keyedEvents[E <: Event: ClassTag: TypeTag](implicit s: Scheduler): Seq[KeyedEvent[E]] =
    eventWatch.all[E].strict match {
      case EventSeq.NonEmpty(stamped) => stamped map (_.value)
      case EventSeq.Empty(_) => Nil
      case TearableEventSeq.Torn(eventId) => throw new TornException(after = EventId(0), tornEventId = eventId)
    }

  /** TEST ONLY - Blocking. */
  @TestOnly
  def all[E <: Event: ClassTag](implicit s: Scheduler, E: TypeTag[E]): TearableEventSeq[Seq, KeyedEvent[E]] =
    eventWatch.all[E].strict

  @inline
  private def delegate[A](body: EventWatch => Task[TearableEventSeq[CloseableIterator, A]]): Task[TearableEventSeq[Seq, A]] =
    body(eventWatch) map (_.strict)

  def tornEventId = eventWatch.tornEventId

  def lastAddedEventId = eventWatch.lastAddedEventId
}
