package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.common.event.EventReader.Every
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, KeyedEvent, SomeEventRequest, Stamped, TearableEventSeq}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable.Seq
import scala.concurrent.duration.{FiniteDuration, _}
import scala.reflect.ClassTag

/**
  * Strict delegator for lazy `CloseableIterator` `EventReader`.
  * Converts `CloseableIterator` to a `Seq` and closes the iterator.
  *
  * @author Joacim Zschimmer
  */
final class StrictEventReader[E <: Event](eventReader: EventReader[E])
{
  def observe[E1 <: E](request: EventRequest[E1], predicate: KeyedEvent[E1] ⇒ Boolean = (_: KeyedEvent[E1]) ⇒ true)
    (implicit scheduler: Scheduler)
  : Observable[Stamped[KeyedEvent[E1]]]
  = eventReader.observe(request, predicate)

  def read[E1 <: E](request: SomeEventRequest[E1], predicate: KeyedEvent[E1] ⇒ Boolean = Every)
  : Task[TearableEventSeq[Seq, KeyedEvent[E1]]]
  = delegate(_.read(request, predicate))

  def when[E1 <: E](request: EventRequest[E1], predicate: KeyedEvent[E1] ⇒ Boolean = Every)
  : Task[TearableEventSeq[Seq, KeyedEvent[E1]]]
  = delegate(_.when(request, predicate))

  def whenAny[E1 <: E](
    request: EventRequest[E1],
    eventClasses: Set[Class[_ <: E1]],
    predicate: KeyedEvent[E1] ⇒ Boolean = Every)
  : Task[TearableEventSeq[Seq, KeyedEvent[E1]]]
  = delegate(_.whenAny[E1](request, eventClasses, predicate))

  def byKey[E1 <: E](
    request: SomeEventRequest[E1],
    key: E1#Key,
    predicate: E1 ⇒ Boolean = Every)
  : Task[TearableEventSeq[Seq, E1]]
  = delegate(_.byKey(request, key, predicate))

  def whenKeyedEvent[E1 <: E](
    request: EventRequest[E1],
    key: E1#Key,
    predicate: E1 ⇒ Boolean = Every)
  : Task[E1]
  = eventReader.whenKeyedEvent(request, key, predicate)

  def whenKey[E1 <: E](
    request: EventRequest[E1],
    key: E1#Key,
    predicate: E1 ⇒ Boolean = Every)
  : Task[TearableEventSeq[Seq, E1]]
  = delegate(_.whenKey(request, key, predicate))

  /** TEST ONLY - Blocking. */
  @TestOnly
  def await[E1 <: E: ClassTag](
    predicate: KeyedEvent[E1] ⇒ Boolean = Every,
    after: EventId = EventId.BeforeFirst,
    timeout: FiniteDuration = 99.seconds)
    (implicit s: Scheduler)
  : Vector[Stamped[KeyedEvent[E1]]]
  = eventReader.await(predicate, after, timeout)

  /** TEST ONLY - Blocking. */
  @TestOnly
  def all[E1 <: E: ClassTag](implicit s: Scheduler): TearableEventSeq[Seq, KeyedEvent[E1]] =
    eventReader.all[E1].strict

  @inline
  private def delegate[A](body: EventReader[E] ⇒ Task[TearableEventSeq[CloseableIterator, A]]): Task[TearableEventSeq[Seq, A]] =
    body(eventReader) map (_.strict)
}
