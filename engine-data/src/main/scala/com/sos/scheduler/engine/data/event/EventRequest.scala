package com.sos.scheduler.engine.data.event

import com.sos.scheduler.engine.base.utils.ScalaUtils.implicitClass
import java.time.Duration
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final case class EventRequest[E <: Event](
  eventClass: Class[E],
  after: EventId,
  timeout: Duration,
  limit: Int)
extends SomeEventRequest {
  require(limit > 0, "Limit must not be below zero")
}

object EventRequest {
  def apply[E <: Event: ClassTag](after: EventId, timeout: Duration, limit: Int = Int.MaxValue): EventRequest[E] =
    new EventRequest(implicitClass[E], after, timeout, limit)
}

final case class ReverseEventRequest[E <: Event](
  eventClass: Class[E],
  limit: Int,
  after: EventId)
extends SomeEventRequest {
  require(limit > 0, "Limit must not be below zero")
}

object ReverseEventRequest {
  def apply[E <: Event: ClassTag](after: EventId = EventId.BeforeFirst, limit: Int = Int.MaxValue): ReverseEventRequest[E] =
    new ReverseEventRequest(implicitClass[E], after = after, limit = limit)
}

sealed trait SomeEventRequest
