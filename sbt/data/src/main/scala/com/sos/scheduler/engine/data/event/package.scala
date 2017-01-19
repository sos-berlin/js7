package com.sos.scheduler.engine.data

import java.time.format.DateTimeFormatter._
import java.time.format.DateTimeFormatterBuilder
import java.time.{Instant, ZoneId}
import spray.json.{JsNumber, JsValue}

/**
  * @author Joacim Zschimmer
  */
package object event {

  /**
    *  Identifies [[com.sos.scheduler.engine.data.event.Snapshot]]s taken at different times.
    *  <p>
    *    The ID encodes the timestamp as the value of milliseconds since 1970-01-01 UTC multiplied by 1000.
    *    The accuracy is one millisecond in most cases (below 1000 events/ms).
    *  <p>
    *    The factor of 1000 is chosen to have room for a counter, starting every millisecond at 0,
    *    to discriminate multiple events of the same millisecond.
    *    The counter may overflow into the millisecond part (at >= 1000 events/s),
    *    in which case the accuracy is no longer a millisecond.
    *    For the algorithm, see EventIdGenerator.
    *
    */
  type EventId = Long

  object EventId {
    val BeforeFirst: EventId = 0
    val IdsPerMillisecond = 1000
    // JavaScript uses floating point for all numbers, so it have 11 bits for a precise integer to
    // represent all integers between 0 and 2**53 (9.007.199.254.740.992).
    // 2 ** 53 = 9.007.199.254.740.992µs = 285 years. This is good until year 2255, for a million events/s.
    private[event] val JsonMaxValue = EventId(1L << 53)  // 2^53 == 9007199254740992L
    val MaxValue = EventId(Long.MaxValue)

    def apply(eventId: String) = eventId.toLong

    def apply(eventId: Long) = eventId

    def toInstant(id: EventId) = Instant ofEpochMilli id / 1000 plusNanos id % 1000 * 1000

    def toJsValue(eventId: EventId) = JsNumber(eventId)

    def fromJsValue(o: JsValue) = o.asInstanceOf[JsNumber].value.toLongExact

    private val UTC = ZoneId of "UTC"
    private val dateTimeFormatter =
      new DateTimeFormatterBuilder()
        .append(ISO_LOCAL_DATE_TIME)
        .optionalStart
        .appendOffsetId
        .optionalStart
        .toFormatter

    def toString(eventId: EventId): String =
      s"$eventId (${toDateTimeString(eventId)})"

    def toDateTimeString(eventId: EventId): String =
      dateTimeFormatter.format(java.time.ZonedDateTime.ofInstant(toInstant(eventId), UTC))
  }

  type AnyKeyedEvent = KeyedEvent[Event]
  type AnyEvent = Event { type Key = Any }
}
