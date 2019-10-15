package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.common.event.EventWatch.Every
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable.Seq
import scala.concurrent.duration.{FiniteDuration, _}
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

/**
  * Strict delegator for lazy `CloseableIterator` `EventWatch`.
  * Converts `CloseableIterator` to a `Seq` and closes the iterator.
  *
  * @author Joacim Zschimmer
  */
final class StrictEventWatch[E <: Event](eventWatch: EventWatch[E])
{
  def observe[E1 <: E](request: EventRequest[E1], predicate: KeyedEvent[E1] => Boolean = (_: KeyedEvent[E1]) => true, onlyLastOfChunk: Boolean)
  : Observable[Stamped[KeyedEvent[E1]]]
  = eventWatch.observe(request, predicate, onlyLastOfChunk)

  def read[E1 <: E](request: EventRequest[E1], predicate: KeyedEvent[E1] => Boolean = Every)
  : Task[TearableEventSeq[Seq, KeyedEvent[E1]]]
  = delegate(_.read(request, predicate))

  def when[E1 <: E](request: EventRequest[E1], predicate: KeyedEvent[E1] => Boolean = Every)
  : Task[TearableEventSeq[Seq, KeyedEvent[E1]]]
  = delegate(_.when(request, predicate))

  def whenAny[E1 <: E](
    request: EventRequest[E1],
    eventClasses: Set[Class[_ <: E1]],
    predicate: KeyedEvent[E1] => Boolean = Every)
  : Task[TearableEventSeq[Seq, KeyedEvent[E1]]]
  = delegate(_.whenAny[E1](request, eventClasses, predicate))

  def byKey[E1 <: E](
    request: EventRequest[E1],
    key: E1#Key,
    predicate: E1 => Boolean = Every)
  : Task[TearableEventSeq[Seq, E1]]
  = delegate(_.byKey(request, key, predicate))

  def whenKeyedEvent[E1 <: E](
    request: EventRequest[E1],
    key: E1#Key,
    predicate: E1 => Boolean = Every)
  : Task[E1]
  = eventWatch.whenKeyedEvent(request, key, predicate)

  def whenKey[E1 <: E](
    request: EventRequest[E1],
    key: E1#Key,
    predicate: E1 => Boolean = Every)
  : Task[TearableEventSeq[Seq, E1]]
  = delegate(_.whenKey(request, key, predicate))

  /** TEST ONLY - Blocking. */
  @TestOnly
  def await[E1 <: E: ClassTag](
    predicate: KeyedEvent[E1] => Boolean = Every,
    after: EventId = EventId.BeforeFirst,
    timeout: FiniteDuration = 99.seconds)
    (implicit s: Scheduler, E1: TypeTag[E1])
  : Vector[Stamped[KeyedEvent[E1]]]
  = eventWatch.await(predicate, after, timeout)

  /** TEST ONLY - Blocking. */
  @TestOnly
  def keyedEvents[E1 <: E: ClassTag: TypeTag](key: E1#Key)(implicit s: Scheduler): Seq[E1] =
    keyedEvents[E1] collect {
      case o if o.key == key => o.event
    }

  /** TEST ONLY - Blocking. */
  @TestOnly
  def keyedEvents[E1 <: E: ClassTag: TypeTag](implicit s: Scheduler): Seq[KeyedEvent[E1]] =
    eventWatch.all[E1].strict match {
      case EventSeq.NonEmpty(stamped) => stamped map (_.value)
      case EventSeq.Empty(_) => Nil
      case TearableEventSeq.Torn(eventId) => throw new TornException(after = EventId(0), tornEventId = eventId)
    }

  /** TEST ONLY - Blocking. */
  @TestOnly
  def all[E1 <: E: ClassTag](implicit s: Scheduler, E1: TypeTag[E1]): TearableEventSeq[Seq, KeyedEvent[E1]] =
    eventWatch.all[E1].strict

  @inline
  private def delegate[A](body: EventWatch[E] => Task[TearableEventSeq[CloseableIterator, A]]): Task[TearableEventSeq[Seq, A]] =
    body(eventWatch) map (_.strict)

  def tornEventId = eventWatch.tornEventId

  def lastAddedEventId = eventWatch.lastAddedEventId
}
