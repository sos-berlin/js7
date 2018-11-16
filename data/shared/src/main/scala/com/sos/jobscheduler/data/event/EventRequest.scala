package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import com.sos.jobscheduler.data.event.EventRequest._
import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, duration}
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final case class EventRequest[E <: Event](
  eventClasses: Set[Class[_ <: E]],
  after: EventId,
  timeout: Duration,
  delay: FiniteDuration = DefaultDelay,
  limit: Int = DefaultLimit,
  tornOlder: Duration = Duration.Inf)
extends SomeEventRequest[E]
{
  require(eventClasses.nonEmpty, "Missing Event class")
  require(limit >= 0, "Limit must not be below zero")

  def toQueryParameters: Vector[(String, String)] = {
    val builder = Vector.newBuilder[(String, String)]
    builder += returnQueryParameter
    if (timeout != Duration.Zero) builder += "timeout" → durationToString(timeout)
    if (delay != DefaultDelay) builder += "delay" → durationToString(delay)
    if (limit != DefaultLimit) builder += "limit" → limit.toString
    if (tornOlder != Duration.Inf) builder += "tornOlder" → durationToString(tornOlder)
    builder += "after" → after.toString
    builder.result()
  }

  /**
    * Helper to repeatedly fetch events until a condition (PartialFunction) is met.
    * Blocking - for testing.
    */
  @tailrec
  def repeat[A](fetchEvents: EventRequest[E] ⇒ Future[TearableEventSeq[Seq, KeyedEvent[E]]])(collect: PartialFunction[Stamped[KeyedEvent[E]], A]): Seq[A] = {
    val waitTimeout = duration.Duration(timeout.toMillis + 10000, duration.MILLISECONDS)
    Await.result(fetchEvents(this), waitTimeout) match {
      case EventSeq.NonEmpty(stampeds) ⇒
        stampeds.collect(collect) match {
          case Seq() ⇒ copy[E](after = stampeds.last.eventId).repeat(fetchEvents)(collect)
          case o ⇒ o
        }
      case EventSeq.Empty(lastEventId) ⇒
        copy[E](after = lastEventId).repeat(fetchEvents)(collect)

      case torn: TearableEventSeq.Torn ⇒
        sys.error(s"Unexpected $torn")
    }
  }
}

object EventRequest {
  val DefaultDelay = Duration.Zero
  val DefaultLimit = Int.MaxValue
  val LongTimeout = 365.days

  /**
    * Convenience for only one Event class.
    */
  def singleClass[E <: Event: ClassTag](
    /** Begin with events after `after`. `after` must be a known EventId. */
    after: EventId = EventId.BeforeFirst,
    /** Wait not longer then `timeout` for events. */
    timeout: Duration = Duration.Zero,
    /** Delay after the first event to collect more events at once. **/
    delay: FiniteDuration = DefaultDelay,
    /** Limit the number of events. */
    limit: Int = DefaultLimit,
    /** Return Torn if the first event is older than `tornOlder`. */
    tornOlder: Duration = Duration.Inf)
  : EventRequest[E] =
    new EventRequest[E](Set(implicitClass[E]), after, timeout, delay, limit, tornOlder)

  private def durationToString(duration: Duration): String =
    duration match {
      case Duration.Inf ⇒ "infinite"
      case duration: FiniteDuration ⇒
        BigDecimal(duration.toNanos, scale = 9).toString.reverse.dropWhile(_ == '0').reverse.stripSuffix(".")  // TODO Use ScalaTime.formatNumber
    }
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
