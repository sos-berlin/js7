package js7.data.event

import fs2.Stream
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.Subtype
import js7.base.utils.MoreJavaConverters.*
import js7.base.utils.MultipleLinesBracket.streamInBrackets
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.data.event.EventCounter.*
import scala.collection.mutable

final case class EventCounter(eventToCount: Map[String, Long], totalEventCount: Long)
extends EventCounterMXBean:

  def applyKeyedEvent(keyedEvent: AnyKeyedEvent): EventCounter =
    // Prefer caching simpleScalaName over keyedEventJsonCodec.classToName, because it works safely.
    val eventName = keyedEvent.event.getClass.simpleScalaName
    copy(
      eventToCount = eventToCount.updatedWith(eventName):
        _.fold(Some(1L))(n => Some(n + 1)),
      totalEventCount = totalEventCount + 1)

  def toStringStream: Stream[fs2.Pure, String] =
    streamInBrackets(s"eventToCount $totalEventCount events"):
      eventToCount.view.map: (name, v) =>
        s"$name $v×"

  def estimatedSnapshotSize: Int =
    eventToCount.size

  def toSnapshotStream: fs2.Stream[fs2.Pure, EventCount] =
    fs2.Stream.iterable:
      eventToCount.view.map((name, n) => EventCount(name, n))

  def appendToPrometheus(sb: StringBuilder): Unit =
    eventToCount.foreach: (name, n) =>
      sb.append("js7_event_total{key=\"").append(name).append("\"} ")
        .append(n).append('\n')


object EventCounter:
  val empty: EventCounter =
    new EventCounter(Map.empty, 0L)

  def apply(eventToCount: Map[String, Long]): EventCounter =
    new EventCounter(eventToCount, eventToCount.values.sum)

  final case class EventCount(eventName: String, count: Long)

  object EventCount:
    private given Codec.AsObject[EventCount] = deriveCodec[EventCount]

    val subtype: Subtype[EventCount] =
      Subtype[EventCount]

    def apply(cls: Class[? <: Event], n: Long) =
      new EventCount(cls.simpleScalaName, n)


  final class Builder:
    private val eventToCount = mutable.Map.empty[String, Long]

    def put(eventName: String, n: Long): Unit =
      eventToCount.put(eventName, n)

    def result(): EventCounter =
      EventCounter(eventToCount.toMap)


sealed trait EventCounterMXBean:
  this: EventCounter =>

  def getEventToTotal: java.util.Map[String, java.lang.Long] =
    eventToCount.view.mapValues(Long.box).asJava

  def getEventTotal: Long =
    totalEventCount
