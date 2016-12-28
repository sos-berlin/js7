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
extends SomeEventRequest[E] {
  require(limit > 0, "Limit must not be below zero")

  def toQueryParameters: Vector[(String, String)] = {
    val builder = Vector.newBuilder[(String, String)]
    builder += "return" → (eventClass.getSimpleName stripSuffix "$")
    if (timeout != Duration.ZERO) builder += "timeout" → timeout.toString
    if (limit != Int.MaxValue) builder += "limit" → limit.toString
    builder += "after" → after.toString
    builder.result()
  }
}

object EventRequest {
  def apply[E <: Event: ClassTag](after: EventId, timeout: Duration, limit: Int = Int.MaxValue): EventRequest[E] =
    new EventRequest(implicitClass[E], after, timeout, limit)
}

final case class ReverseEventRequest[E <: Event](
  eventClass: Class[E],
  limit: Int,
  after: EventId)
extends SomeEventRequest[E] {
  require(limit > 0, "Limit must not be below zero")

  def toQueryParameters: Vector[(String, String)] = {
    val builder = Vector.newBuilder[(String, String)]
    if (eventClass != classOf[Event]) builder += "return" → (eventClass.getSimpleName stripSuffix "$")
    builder += "limit" → (-limit).toString
    if (after != EventId.BeforeFirst) builder += "after" → after.toString
    builder.result()
  }
}

object ReverseEventRequest {
  def apply[E <: Event: ClassTag](after: EventId = EventId.BeforeFirst, limit: Int): ReverseEventRequest[E] =
    new ReverseEventRequest(implicitClass[E], after = after, limit = limit)
}

/**
  * Common trait for both EventRequest and ReverseEventRequest.
  */
sealed trait SomeEventRequest[E <: Event] {
  def eventClass: Class[E]

  def toQueryParameters: Vector[(String, String)]
}
