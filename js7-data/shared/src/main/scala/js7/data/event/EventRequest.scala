package js7.data.event

import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.EventRequest.*
import scala.concurrent.duration.*
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final case class EventRequest[E <: Event](
  eventClasses: Set[Class[? <: E]],
  after: EventId,
  timeout: Option[FiniteDuration],
  delay: FiniteDuration = DefaultDelay,
  limit: Int = DefaultLimit,
  tornOlder: Option[FiniteDuration] = None)
{
  require(eventClasses.nonEmpty, "Missing Event class")
  require(limit >= 0, s"EventRequest limit=$limit must not be below zero")

  def toQueryParameters: Vector[(String, String)] = {
    val builder = Vector.newBuilder[(String, String)]
    builder += "return" -> eventClasses.map(_.getSimpleName stripSuffix "$").mkString(",")
    builder += "delay" -> durationToString(delay)
    for (o <- timeout) builder += "timeout" -> durationToString(o)
    if (limit != DefaultLimit) builder += "limit" -> limit.toString
    for (o <- tornOlder) builder += "tornOlder" -> durationToString(o)
    builder += "after" -> after.toString
    builder.result()
  }

  def matchesClass(clazz: Class[? <: Event]): Boolean =
    eventClasses.exists(_ isAssignableFrom clazz)
}

object EventRequest
{
  private val DefaultDelay = ZeroDuration
  private val DefaultLimit = Int.MaxValue
  val LongTimeout = 365.days

  /**
    * Convenience for only one Event class.
    */
  def singleClass[E <: Event: ClassTag](
    /** Begin with events after `after`. `after` must be a known EventId. */
    after: EventId = EventId.BeforeFirst,
    /** Wait not longer then `timeout` for events. */
    timeout: Option[FiniteDuration] = Some(ZeroDuration),
    /** Delay after the first event to collect more events at once. **/
    delay: FiniteDuration = DefaultDelay,
    /** Limit the number of events. */
    limit: Int = DefaultLimit,
    /** Return Torn if the first event is older than `tornOlder`. */
    tornOlder: Option[FiniteDuration] = None)
  : EventRequest[E] = {
    if (implicitClass[E] eq classOf[Nothing])
      throw new IllegalArgumentException("EventRequest.singleClass[Nothing]: Missing type parameter?")
    new EventRequest[E](Set(implicitClass[E]), after, timeout, delay, limit, tornOlder)
  }

  def durationToString(duration: FiniteDuration): String =
    BigDecimal(duration.toNanos, scale = 9).bigDecimal.toPlainString.dropLastWhile(_ == '0').stripSuffix(".")  // TODO Use ScalaTime.formatNumber
}
