package com.sos.jobscheduler.data

import com.sos.jobscheduler.base.time.Timestamp

/**
  * @author Joacim Zschimmer
  */
package object event {

  /**
    *  Identifies [[com.sos.jobscheduler.data.event.Stamped]]s taken at different times.
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

  type AnyKeyedEvent = KeyedEvent[Event]
  type AnyEvent = Event { type Key = Any }

  val <-: = KeyedEvent

  object EventId {
    val BeforeFirst: EventId = 0
    val IdsPerMillisecond = 1000
    // JavaScript uses floating point for all numbers, so it have 11 bits for a precise integer to
    // represent all integers between 0 and 2**53 (9.007.199.254.740.992).
    // 2 ** 53 = 9.007.199.254.740.992µs = 285 years. This is good until year 2255, for a million events/s.
    private[event] val JavascriptMaxValue = EventId(1L << 53)  // 2^53 == 9007199254740992L
    val MaxValue = EventId(Long.MaxValue)

    def apply(eventId: String) = eventId.toLong

    @inline
    def apply(eventId: Long) = eventId

    def toTimestamp(eventId: EventId) = Timestamp ofEpochMilli(toEpochMilli(eventId))

    def toEpochMilli(eventId: EventId) = eventId / IdsPerMillisecond

    def toString(eventId: EventId): String =
      s"$eventId/${toDateTimeString(eventId)}"

    def toDateTimeString(eventId: EventId): String =
      eventId match {
        case BeforeFirst => "BeforeFirst"
        case _ =>
          val millis = eventId / 1000
          val micros = eventId % 1000
          val iso = Timestamp.ofEpochMilli(millis).toIsoString  // Timestamp has millisecond presision
          if (micros == 0)
            iso
          else {
            val sb = new StringBuilder(iso.length + 10)
            sb ++= iso
            sb.deleteCharAt(sb.length - 1)  // 'Z'
            if (!sb.contains('.')) sb ++= ".000"
            sb += ('0' + micros / 100).toChar
            sb += ('0' + micros / 10 % 10).toChar
            sb += ('0' + micros % 10).toChar
            sb += 'Z'
            sb.toString
          }
      }
  }
}
