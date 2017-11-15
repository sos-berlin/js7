package com.sos.jobscheduler.master.gui.data


/**
  * @author Joacim Zschimmer
  */
package object event {

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

    def toTimestamp(id: EventId): Timestamp =
      Timestamp.fromEpochMicro(id)

    def toString(eventId: EventId): String =
      s"$eventId(${toDateTimeString(eventId)})"

    def toDateTimeString(eventId: EventId): String =
      eventId match {
        case BeforeFirst ⇒ "BeforeFirst"
        case _ ⇒ toTimestamp(eventId).toString
      }
  }
}
