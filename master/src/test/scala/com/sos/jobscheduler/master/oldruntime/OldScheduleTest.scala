package com.sos.scheduler.engine.master.oldruntime

import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.master.oldruntime.OldSchedule.EveryDay
import java.time.{LocalTime, ZoneId, ZonedDateTime}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OldScheduleTest extends FreeSpec {

  private val timeZone = ZoneId.of("Europe/Helsinki")
  private val oldSchedule = OldSchedule(timeZone, PeriodSeq(List(
        Period(begin = LocalTime.of(9, 0), end = ExtendedLocalTime.of(10, 0), absoluteRepeat = Some(20*60.s)),
        Period(begin = LocalTime.of(12, 0), end = ExtendedLocalTime.of(12, 31), absoluteRepeat = Some(30*60.s)))), startOnce = false)

  "firstInstant" - {
    for ((from, next) ← Array(
      "2017-01-06T06:00:00+02:00" → "2017-01-06T09:00:00+02:00",
      "2017-01-06T09:00:00+02:00" → "2017-01-06T09:20:00+02:00",
      "2017-01-06T11:00:00+02:00" → "2017-01-06T12:00:00+02:00",
      "2017-01-06T20:00:00+02:00" → "2017-01-07T09:00:00+02:00"
    )) from in {
      assert(oldSchedule.firstInstant(instant(from)) == Some(instant(next)))
    }
  }

  "EveryDay" in {
    val a = EveryDay(PeriodSeq(Nil))
    val b = EveryDay(PeriodSeq(Nil))
    assert(a == b)
  }

  private def instant(string: String) = ZonedDateTime.parse(string).toInstant
}
