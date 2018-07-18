package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.base.utils.ScalaUtils.function1WithToString
import com.sos.jobscheduler.common.event.EventReader.Every
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, KeyedEvent, SomeEventRequest, Stamped, TearableEventSeq}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration._
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
trait EventReader[E <: Event] {

  def strict: StrictEventReader[E] = new StrictEventReader(this)

  def observe[E1 <: E](request: EventRequest[E1], predicate: KeyedEvent[E1] ⇒ Boolean = Every)
    (implicit scheduler: Scheduler)
  : Observable[Stamped[KeyedEvent[E1]]]

  def read[E1 <: E](request: SomeEventRequest[E1], predicate: KeyedEvent[E1] ⇒ Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E1]]]

  def when[E1 <: E](request: EventRequest[E1], predicate: KeyedEvent[E1] ⇒ Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E1]]]

  def whenAny[E1 <: E](
    request: EventRequest[E1],
    eventClasses: Set[Class[_ <: E1]],
    predicate: KeyedEvent[E1] ⇒ Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E1]]]

  def byKey[E1 <: E](
    request: SomeEventRequest[E1],
    key: E1#Key,
    predicate: E1 ⇒ Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, E1]]

  def whenKeyedEvent[E1 <: E](
    request: EventRequest[E1],
    key: E1#Key,
    predicate: E1 ⇒ Boolean = Every)
  : Task[E1]

  def whenKey[E1 <: E](
    request: EventRequest[E1],
    key: E1#Key,
    predicate: E1 ⇒ Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, E1]]

  /** TEST ONLY - Blocking. */
  @TestOnly
  def await[E1 <: E: ClassTag](
    predicate: KeyedEvent[E1] ⇒ Boolean = Every,
    after: EventId = EventId.BeforeFirst,
    timeout: FiniteDuration = 99.seconds)
    (implicit s: Scheduler)
  : Vector[Stamped[KeyedEvent[E1]]]

  /** TEST ONLY - Blocking. */
  @TestOnly
  def all[E1 <: E: ClassTag](implicit s: Scheduler): TearableEventSeq[CloseableIterator, KeyedEvent[E1]]

  def tornEventId: EventId

  def lastAddedEventId: EventId
}

object EventReader
{
  private[event] val Every: Any ⇒ Boolean = function1WithToString("Every")(_ ⇒ true)
}
