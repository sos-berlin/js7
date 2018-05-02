package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.common.event.{EventReader, RealEventReader}
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, KeyedEvent, SomeEventRequest}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

/**
  * Delegator for a later provided `EventReader`.
  *
  * @author Joacim Zschimmer
  */
trait EventReaderProvider[E <: Event] extends EventReader[E]
{
  private lazy val realEventReaderTask = Task.fromFuture(whenRealEventReader)

  def observe[E1 <: E](request: EventRequest[E1], predicate: KeyedEvent[E1] ⇒ Boolean = (_: KeyedEvent[E1]) ⇒ true)
    (implicit scheduler: Scheduler)
  = Observable.fromFuture(whenRealEventReader) flatMap (_.observe(request, predicate))

  def read[E1 <: E](request: SomeEventRequest[E1], predicate: KeyedEvent[E1] ⇒ Boolean) =
    delegate(_.read(request, predicate))

  final def whenForKey[E1 <: E](request: EventRequest[E1], key: E1#Key, predicate: E1 ⇒ Boolean) =
    delegate(_.whenKey(request, key, predicate))

  final def whenKeyedEvent[E1 <: E](request: EventRequest[E1], key: E1#Key, predicate: E1 ⇒ Boolean) =
    delegate(_.whenKeyedEvent(request, key, predicate))

  final def whenKey[E1 <: E](
    request: EventRequest[E1],
    key: E1#Key,
    predicate: E1 ⇒ Boolean)
  =
    delegate(_.whenKey(request, key, predicate))

  def byKey[E1 <: E](request: SomeEventRequest[E1], key: E1#Key, predicate: E1 ⇒ Boolean) =
    delegate(_.byKey(request, key, predicate))

  def whenAny[E1 <: E](
    request: EventRequest[E1],
    eventClasses: Set[Class[_ <: E1]],
    predicate: KeyedEvent[E1] ⇒ Boolean)
  =
    delegate(_.whenAny[E1](request, eventClasses, predicate))

  final def when[E1 <: E](
    request: EventRequest[E1],
    predicate: KeyedEvent[E1] ⇒ Boolean)
  =
    delegate(_.when(request, predicate))

  @TestOnly
  final def eventsAfter(after: EventId) =
    delegate(_.eventsAfter(after))

  /** TEST ONLY - Blocking. */
  @TestOnly
  def await[E1 <: E: ClassTag](predicate: KeyedEvent[E1] ⇒ Boolean, after: EventId, timeout: FiniteDuration)(implicit s: Scheduler) =
    whenRealEventReader.await(timeout).await(predicate, after, timeout)

  /** TEST ONLY - Blocking. */
  @TestOnly
  def all[E1 <: E: ClassTag](implicit s: Scheduler) =
    whenRealEventReader.await(99.s).all

  private def delegate[A](body: RealEventReader[E] ⇒ Task[A]) =
    realEventReaderTask flatMap body
}
