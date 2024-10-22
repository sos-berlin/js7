package js7.data.event

import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.implicitClass
import js7.data.event.EventRequest.*
import scala.concurrent.duration.*
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final case class EventRequest[E <: Event](
  eventClasses: Set[Class[? <: E]],
  after: EventId,
  // TODO Simplify timeout to something like lastEventsOnly: Boolean?
  //  Entweder nur anstehende Events oder endloser Strom
  timeout: Option[FiniteDuration],
  // TODO Do we need delay?
  delay: FiniteDuration = DefaultDelay,
  limit: Int = DefaultLimit,
  tornOlder: Option[FiniteDuration] = None):

  require(eventClasses.nonEmpty, "Missing Event class")
  require(limit >= 0, s"EventRequest limit=$limit must not be below zero")

  def toQueryParameters: Vector[(String, String)] =
    val builder = Vector.newBuilder[(String, String)]
    builder += "return" -> eventClasses.map(_.getSimpleName stripSuffix "$").mkString(",")
    builder += "delay" -> delay.toDecimalString
    for o <- timeout do builder += "timeout" -> o.toDecimalString
    if limit != DefaultLimit then builder += "limit" -> limit.toString
    for o <- tornOlder do builder += "tornOlder" -> o.toDecimalString
    builder += "after" -> after.toString
    builder.result()

  def matchesClass(clazz: Class[? <: Event]): Boolean =
    eventClasses.exists(_ isAssignableFrom clazz)


object EventRequest:
  private val DefaultDelay = ZeroDuration
  private val DefaultLimit = Int.MaxValue
  val LongTimeout: FiniteDuration = 365.days

  /**
   * Convenience for only one Event class.
   * @param after  Begin with events after `after`. `after` must be a known EventId.
   * @param timeout timeout Wait no longer then `timeout` for events
   * @param delay Delay after the first event to collect more events at once
   * @param limit Limit the number of events
   * @param tornOlder  Return Torn if the first event is older than `tornOlder`
   */
  def singleClass[E <: Event: ClassTag](
    after: EventId = EventId.BeforeFirst,
    timeout: Option[FiniteDuration]/* = Some(ZeroDuration)*/,
    delay: FiniteDuration = DefaultDelay,
    limit: Int = DefaultLimit,
    tornOlder: Option[FiniteDuration] = None)
  : EventRequest[E] =
    if implicitClass[E] eq classOf[Nothing] then
      throw new IllegalArgumentException("EventRequest.singleClass[Nothing]: Missing type parameter?")
    new EventRequest[E](Set(implicitClass[E]), after, timeout, delay, limit, tornOlder)
