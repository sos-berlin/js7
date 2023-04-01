package js7.data.event

import js7.base.time.Timestamp

object EventId
{
  val BeforeFirst: EventId = 0L
  val IdsPerMillisecond = 1000
  // JavaScript uses floating point for all numbers, so it have 11 bits for a precise integer to
  // represent all integers between 0 and 2**53 (9.007.199.254.740.992).
  // 2 ** 53 = 9.007.199.254.740.992µs = 285 years. This is good until year 2255, for a million events/s.
  private[event] val JavascriptMaxValue = EventId(1L << 53)  // 2^53 == 9007199254740992L
  private[js7] val MaxValue = EventId(JavascriptMaxValue)

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
        if (true || micros == 0)
          iso
        else {
          val sb = new StringBuilder(iso.length + 10)
          sb ++= iso
          sb.deleteCharAt(sb.length - 1)  // 'Z'
          if (!sb.contains('.')) sb ++= ".000"
          sb ++= "Z-"
          sb += ('0' + micros / 100).toChar
          sb += ('0' + micros / 10 % 10).toChar
          sb += ('0' + micros % 10).toChar
          sb.toString
        }
    }
}
