package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.common.event.EventReader
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, KeyedEvent, SomeEventRequest}
import java.time.Duration
import monix.eval.Task
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly
import scala.reflect.ClassTag

/**
  * Delegator for a later provided `EventReader`.
  *
  * @author Joacim Zschimmer
  */
trait EventReaderProvider[E <: Event] extends EventReader[E]
{
  def whenEventReader: Task[JournalEventReader[E]]

  def read[E1 <: E](request: SomeEventRequest[E1], predicate: KeyedEvent[E1] ⇒ Boolean) =
    whenEventReader flatMap (_.read(request, predicate))

  final def whenForKey[E1 <: E](request: EventRequest[E1], key: E1#Key, predicate: E1 ⇒ Boolean) =
    whenEventReader flatMap (_.whenKey(request, key, predicate))

  final def whenKeyedEvent[E1 <: E](request: EventRequest[E1], key: E1#Key, predicate: E1 ⇒ Boolean) =
    whenEventReader flatMap (_.whenKeyedEvent(request, key, predicate))

  final def whenKey[E1 <: E](
    request: EventRequest[E1],
    key: E1#Key,
    predicate: E1 ⇒ Boolean)
  =
    whenEventReader flatMap (_.whenKey(request, key, predicate))

  def byKey[E1 <: E](request: SomeEventRequest[E1], key: E1#Key, predicate: E1 ⇒ Boolean) =
    whenEventReader flatMap (_.byKey(request, key, predicate))

  def whenAny[E1 <: E](
    request: EventRequest[E1],
    eventClasses: Set[Class[_ <: E1]],
    predicate: KeyedEvent[E1] ⇒ Boolean)
  =
    whenEventReader flatMap (_.whenAny[E1](request, eventClasses, predicate))

  final def when[E1 <: E](
    request: EventRequest[E1],
    predicate: KeyedEvent[E1] ⇒ Boolean)
  =
    whenEventReader flatMap (_.when(request, predicate))

  final def eventsAfter(after: EventId) =
    whenEventReader flatMap (_.eventsAfter(after))

  /** TEST ONLY - Blocking. */
  @TestOnly
  def await[E1 <: E: ClassTag](
    predicate: KeyedEvent[E1] ⇒ Boolean,
    after: EventId = EventId.BeforeFirst,
    timeout: Duration = 99.s)
    (implicit s: Scheduler)
  =
    whenEventReader.await(timeout).await(predicate, after, timeout)

  /** TEST ONLY - Blocking. */
  @TestOnly
  def all[E1 <: E: ClassTag](implicit s: Scheduler) =
    whenEventReader.await(99.s).all
}
