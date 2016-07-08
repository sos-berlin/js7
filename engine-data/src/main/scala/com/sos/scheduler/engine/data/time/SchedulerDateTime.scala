package com.sos.scheduler.engine.data.time

import java.time.format.DateTimeFormatterBuilder
import java.time.temporal.ChronoField._
import java.time.temporal.TemporalAccessor
import java.time.{Instant, ZoneId, ZonedDateTime}

/**
  * @author Joacim Zschimmer
  */
object SchedulerDateTime {
  val formatUtc: TemporalAccessor ⇒ String =
    newDateFormatBuilder().appendLiteral('Z').toFormatter.withZone(ZoneId.of("UTC")).format _
  private val utc = ZoneId.of("UTC")

  private def newDateFormatBuilder() =
    new DateTimeFormatterBuilder()
    .appendValue(YEAR, 4).appendLiteral('-').appendValue(MONTH_OF_YEAR, 2).appendLiteral('-').appendValue(DAY_OF_MONTH, 2)
    .appendLiteral('T')
    .appendValue(HOUR_OF_DAY, 2).appendLiteral(':').appendValue(MINUTE_OF_HOUR, 2).appendLiteral(':').appendValue(SECOND_OF_MINUTE, 2)

  def formatLocally(defaultTimeZoneId: ZoneId, instant: Instant) =
    newDateFormatBuilder().toFormatter.withZone(defaultTimeZoneId).format(ZonedDateTime.ofInstant(instant, utc))
}
