package com.sos.scheduler.engine.data.event

import com.sos.scheduler.engine.base.utils.ScalaUtils.implicitClass
import java.time.Duration
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final case class EventRequest[E <: Event](
  eventClasses: Set[Class[_ <: E]],
  after: EventId,
  timeout: Duration,
  limit: Int)
extends SomeEventRequest[E] {
  require(eventClasses.nonEmpty, "Missing Event class")
  require(limit > 0, "Limit must not be below zero")

  def toQueryParameters: Vector[(String, String)] = {
    val builder = Vector.newBuilder[(String, String)]
    builder += returnQueryParameter
    if (timeout != Duration.ZERO) builder += "timeout" → timeout.toString
    if (limit != Int.MaxValue) builder += "limit" → limit.toString
    builder += "after" → after.toString
    builder.result()
  }
}

object EventRequest {
  /**
    * Convenience for only one Event class.
    */
  def singleClass[E <: Event: ClassTag](after: EventId, timeout: Duration, limit: Int = Int.MaxValue): EventRequest[E] =
    new EventRequest[E](Set(implicitClass[E]), after, timeout, limit)
}

final case class ReverseEventRequest[E <: Event](
  eventClasses: Set[Class[_ <: E]],
  limit: Int,
  after: EventId)
extends SomeEventRequest[E] {
  require(limit > 0, "Limit must not be below zero")

  def toQueryParameters: Vector[(String, String)] = {
    val builder = Vector.newBuilder[(String, String)]
    if (eventClasses != Set(classOf[Event])) builder += returnQueryParameter
    builder += "limit" → (-limit).toString
    if (after != EventId.BeforeFirst) builder += "after" → after.toString
    builder.result()
  }
}

object ReverseEventRequest {
  def apply[E <: Event: ClassTag](after: EventId = EventId.BeforeFirst, limit: Int): ReverseEventRequest[E] =
    new ReverseEventRequest[E](Set(implicitClass[E]), after = after, limit = limit)
}

/**
  * Common trait for both EventRequest and ReverseEventRequest.
  */
sealed trait SomeEventRequest[E <: Event] {
  def eventClasses: Set[Class[_ <: E]]

  def toQueryParameters: Vector[(String, String)]

  protected def returnQueryParameter: (String, String) =
    "return" → (eventClasses map { _.getSimpleName stripSuffix "$" } mkString ",")

  def matchesClass(clazz: Class[_ <: Event]): Boolean =
    eventClasses exists { _ isAssignableFrom clazz }
}
