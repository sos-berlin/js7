package com.sos.jobscheduler.master.oldruntime

import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.master.oldruntime.OldSchedule.EveryDay
import java.time.{DayOfWeek, Instant, LocalTime, ZoneId, ZonedDateTime}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OldScheduleTest extends FreeSpec {

  private val timeZone = ZoneId.of("Europe/Helsinki")

  "Daily, firstInstant" - {
    val dailySchedule = OldSchedule.daily(timeZone, PeriodSeq(List(
      RepeatPeriod(LocalTime.of(9, 0), ExtendedLocalTime.of(10, 0), 20*60.s),
      SingleStartPeriod(LocalTime.of(10, 37)),
      RepeatPeriod(LocalTime.of(12, 0), ExtendedLocalTime.of(12, 31), 30*60.s)))/*, startOnce = false*/)
    for ((from, next) ← Array(
      "2017-01-06T06:00:00+02:00" → "2017-01-06T09:00:00+02:00",
      "2017-01-06T09:00:00+02:00" → "2017-01-06T09:20:00+02:00",
      "2017-01-06T10:00:00+02:00" → "2017-01-06T10:37:00+02:00",
      "2017-01-06T11:00:00+02:00" → "2017-01-06T12:00:00+02:00",
      "2017-01-06T20:00:00+02:00" → "2017-01-07T09:00:00+02:00"
    )) from in {
      assert(dailySchedule.firstInstant(instant(from)) == Some(instant(next)))
    }
  }

  "Weekdays, instants" in {
    val schedule = OldSchedule(timeZone, Map(
      DayOfWeek.MONDAY  → RepeatPeriod(LocalTime.of(1, 0), LocalTime.of(2, 0), 30*60.s),
      DayOfWeek.TUESDAY → SingleStartPeriod(LocalTime.of(2, 2))))
    val interval = instant("2017-01-01T00:00:00+02:00") → instant("2017-01-12T00:00:00+02:00")
    assert(schedule.instants(interval, 100).toList ==
      List(
        instant("2017-01-02T01:00:00+02:00"),
        instant("2017-01-02T01:30:00+02:00"),
        instant("2017-01-03T02:02:00+02:00"),
        instant("2017-01-09T01:00:00+02:00"),
        instant("2017-01-09T01:30:00+02:00"),
        instant("2017-01-10T02:02:00+02:00")))
  }

  "EveryDay" in {
    val a = EveryDay(PeriodSeq(Nil))
    val b = EveryDay(PeriodSeq(Nil))
    assert(a == b)
  }

  private def instant(string: String) = ZonedDateTime.parse(string).toInstant
}
