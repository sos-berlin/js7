package js7.base.time

import java.time.DayOfWeek.TUESDAY
import java.time.{LocalDateTime, LocalTime}
import js7.base.time.AdmissionPeriodForJavaTime._
import js7.base.time.ScalaTime._
import org.scalatest.freespec.AnyFreeSpec

final class AdmissionPeriodForJavaTimeTest extends AnyFreeSpec
{
  "WeekdayPeriod" - {
    val weekdayPeriod = WeekdayPeriod(TUESDAY, LocalTime.of(8, 0), 2.h)
    val admissionPeriod: AdmissionPeriod = weekdayPeriod

    "secondOfWeek" in {
      assert(weekdayPeriod.secondOfWeek == 1*24*3600 + 8*3600)
    }

    "weekday" in {
      assert(weekdayPeriod.dayNumber == 2)
      assert(weekdayPeriod.dayName == "Tuesday")
    }

    "toInterval" in {
      assert(admissionPeriod.toInterval(LocalDateTime.parse("2021-08-23T00:00")) ==
        Some(LocalInterval(LocalDateTime.parse("2021-08-24T08:00"), 2.h)))
    }

    "toInterval may return past period" in {
      // Past periods will be deleted by the caller. For easier code.
      assert(admissionPeriod.toInterval(LocalDateTime.parse("2021-08-29T00:00")) ==
        Some(LocalInterval(LocalDateTime.parse("2021-08-24T08:00"), 2.h)))
    }

    "calendarStart" in {
      assert(admissionPeriod.calendarStart(LocalDateTime.parse("2021-08-23T00:00")) ==
        LocalDateTime.parse("2021-08-23T00:00"))
      assert(admissionPeriod.calendarStart(LocalDateTime.parse("2021-08-29T23:59")) ==
        LocalDateTime.parse("2021-08-23T00:00"))
    }

    "nextCalendarStart" in {
      assert(admissionPeriod.nextCalendarStart(LocalDateTime.parse("2021-08-23T00:00")) ==
        LocalDateTime.parse("2021-08-30T00:00"))
      assert(admissionPeriod.nextCalendarStart(LocalDateTime.parse("2021-08-29T23:59")) ==
        LocalDateTime.parse("2021-08-30T00:00"))
    }
  }
}
