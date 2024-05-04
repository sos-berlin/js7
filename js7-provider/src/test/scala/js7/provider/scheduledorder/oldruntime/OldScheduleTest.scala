package js7.provider.scheduledorder.oldruntime

import java.time.{DayOfWeek, Duration, LocalTime, ZoneId, ZonedDateTime}
import js7.base.test.OurTestSuite
import js7.provider.scheduledorder.oldruntime.OldSchedule.EveryDay

/**
  * @author Joacim Zschimmer
  */
final class OldScheduleTest extends OurTestSuite:

  private val timeZone = ZoneId.of("Europe/Helsinki")

  "Every hour" in:
    val instants = OldSchedule.daily(timeZone, RepeatPeriod.wholeDay(Duration.ofHours(1))).instants(
      instant("2017-03-10T12:00:00+02:00") -> instant("2017-03-10T15:00:00+02:00")).toList
    assert(instants == List(
      instant("2017-03-10T12:00:00+02:00"),
      instant("2017-03-10T13:00:00+02:00"),
      instant("2017-03-10T14:00:00+02:00")))

  "Daily, firstInstant" - {
    val dailySchedule = OldSchedule.daily(timeZone, PeriodSeq(List(
      RepeatPeriod(LocalTime.of(9, 0), ExtendedLocalTime.of(10, 0), Duration.ofMinutes(20)),
      SingleStartPeriod(LocalTime.of(10, 37)),
      RepeatPeriod(LocalTime.of(12, 0), ExtendedLocalTime.of(12, 31), Duration.ofMinutes(30))))/*, startOnce = false*/)
    for ((from, next) <- Array(
      "2017-01-06T06:00:00+02:00" -> "2017-01-06T09:00:00+02:00",
      "2017-01-06T09:00:00+02:00" -> "2017-01-06T09:00:00+02:00",
      "2017-01-06T10:00:00+02:00" -> "2017-01-06T10:37:00+02:00",
      "2017-01-06T11:00:00+02:00" -> "2017-01-06T12:00:00+02:00",
      "2017-01-06T20:00:00+02:00" -> "2017-01-07T09:00:00+02:00"
    )) from in:
      assert(dailySchedule.firstInstant(instant(from)) == Some(instant(next)))
  }

  "Weekdays, instants" in:
    val schedule = OldSchedule(timeZone, Map(
      DayOfWeek.MONDAY  -> RepeatPeriod(LocalTime.of(1, 0), LocalTime.of(2, 0), Duration.ofMinutes(30)),
      DayOfWeek.TUESDAY -> SingleStartPeriod(LocalTime.of(2, 2))))
    val interval = instant("2017-01-01T00:00:00+02:00") -> instant("2017-01-12T00:00:00+02:00")
    assert(schedule.instants(interval, 100).toList ==
      List(
        instant("2017-01-02T01:00:00+02:00"),
        instant("2017-01-02T01:30:00+02:00"),
        instant("2017-01-03T02:02:00+02:00"),
        instant("2017-01-09T01:00:00+02:00"),
        instant("2017-01-09T01:30:00+02:00"),
        instant("2017-01-10T02:02:00+02:00")))

  "EveryDay" in:
    val a = EveryDay(PeriodSeq(Nil))
    val b = EveryDay(PeriodSeq(Nil))
    assert(a == b)

  private def instant(string: String) = ZonedDateTime.parse(string).toInstant
