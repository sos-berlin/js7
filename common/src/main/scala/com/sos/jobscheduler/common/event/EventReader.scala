package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.common.event.EventReader.Every
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, KeyedEvent, SomeEventRequest, Stamped, TearableEventSeq}
import monix.eval.Task
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration._
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
trait EventReader[E <: Event] {

  def read[E1 <: E](request: SomeEventRequest[E1], predicate: KeyedEvent[E1] ⇒ Boolean = Every)
  : Task[TearableEventSeq[Iterator, KeyedEvent[E1]]]

  def when[E1 <: E](request: EventRequest[E1], predicate: KeyedEvent[E1] ⇒ Boolean = Every)
  : Task[TearableEventSeq[Iterator, KeyedEvent[E1]]]

  def whenAny[E1 <: E](
    request: EventRequest[E1],
    eventClasses: Set[Class[_ <: E1]],
    predicate: KeyedEvent[E1] ⇒ Boolean = Every)
  : Task[TearableEventSeq[Iterator, KeyedEvent[E1]]]

  def byKey[E1 <: E](
    request: SomeEventRequest[E1],
    key: E1#Key,
    predicate: E1 ⇒ Boolean = Every)
  : Task[TearableEventSeq[Iterator, E1]]

  def whenKeyedEvent[E1 <: E](
    request: EventRequest[E1],
    key: E1#Key,
    predicate: E1 ⇒ Boolean = Every)
  : Task[E1]

  def whenKey[E1 <: E](
    request: EventRequest[E1],
    key: E1#Key,
    predicate: E1 ⇒ Boolean = Every)
  : Task[TearableEventSeq[Iterator, E1]]

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
  def all[E1 <: E: ClassTag](implicit s: Scheduler): TearableEventSeq[Iterator, KeyedEvent[E1]]
}

object EventReader
{
  private val Every: Any ⇒ Boolean = _ ⇒ true
}
