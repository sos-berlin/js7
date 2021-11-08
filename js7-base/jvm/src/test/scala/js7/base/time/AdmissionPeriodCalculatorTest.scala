package js7.base.time

import java.time.DayOfWeek.{MONDAY, SATURDAY, TUESDAY}
import java.time.LocalTime.MIDNIGHT
import java.time.{LocalDate, LocalDateTime, LocalTime}
import js7.base.time.AdmissionPeriodCalculator.{AlwaysPeriodCalculator, DailyPeriodCalculator, WeekdayPeriodCalculator}
import js7.base.time.ScalaTime._
import org.scalatest.freespec.AnyFreeSpec
import scala.jdk.DurationConverters.ScalaDurationOps

final class AdmissionPeriodCalculatorTest extends AnyFreeSpec
{
  "WeekdayPeriod" - {
    val weekdayPeriod = WeekdayPeriod(TUESDAY, LocalTime.of(3, 0), 2.h)
    // 2021-03-01 is a monday

    "No dateOffset" - {
      val calculator = new WeekdayPeriodCalculator(weekdayPeriod, dateOffset = 0.s.toJava)

      "calendarStart" in {
        assert(calculator.calendarStart(localDT("2021-03-01T00:00")) == localDT("2021-03-01T00:00"))
        assert(calculator.calendarStart(localDT("2021-03-07T23:59")) == localDT("2021-03-01T00:00"))
      }

      "nextCalendarStart" in {
        assert(calculator.nextCalendarStart(localDT("2021-03-01T00:00")) == localDT("2021-03-08T00:00"))
        assert(calculator.nextCalendarStart(localDT("2021-03-07T23:59")) == localDT("2021-03-08T00:00"))
      }

      addWeekdayTests(weekdayPeriod, calculator)
    }

    "dateOffset = 6h" - {
      val calculator = new WeekdayPeriodCalculator(weekdayPeriod, dateOffset = 6.h.toJava)

      "calendarStart" in {
        assert(calculator.calendarStart(localDT("2021-03-01T06:00")) == localDT("2021-03-01T06:00"))
        assert(calculator.calendarStart(localDT("2021-03-08T05:59")) == localDT("2021-03-01T06:00"))
      }

      "nextCalendarStart" in {
        assert(calculator.nextCalendarStart(localDT("2021-03-01T06:00")) == localDT("2021-03-08T06:00"))
        assert(calculator.nextCalendarStart(localDT("2021-03-07T05:59")) == localDT("2021-03-08T06:00"))
      }


      addWeekdayTests(weekdayPeriod, calculator)
    }

    def addWeekdayTests(weekdayPeriod: WeekdayPeriod, calculator: WeekdayPeriodCalculator) = {
      // Results are indifferent to datePeriod

      "secondOfWeek (wall clock)" in {
        assert(weekdayPeriod.secondOfWeek == 1*24*3600 + 3*3600)
      }

      "dayNumber, dayName" in {
        assert(weekdayPeriod.dayOffset == 1/*monday is 0*/)
        assert(weekdayPeriod.dayName == "Tuesday")
      }

      "secondsSinceStart" in {
        assert(calculator.secondsSinceStart(localDT("2021-03-01T00:00")) == -(24+3)*3600)
        assert(calculator.secondsSinceStart(localDT("2021-03-01T01:00")) == -(24+2)*3600)
        assert(calculator.secondsSinceStart(localDT("2021-03-02T00:00")) == -3*3600)
        assert(calculator.secondsSinceStart(localDT("2021-03-02T03:00")) == 0)
        assert(calculator.secondsSinceStart(localDT("2021-03-02T04:00")) == 3600)
        assert(calculator.secondsSinceStart(localDT("2021-03-07T00:00")) == (4*24+21)*3600)
      }

      "toInterval" in {
        assert(calculator.toLocalInterval(localDT("2021-03-01T00:00")) ==
          Some(LocalInterval(localDT("2021-03-02T03:00"), 2.h)))
      }

      "toInterval may return past period" in {
        // Past periods will be deleted by the caller. For easier code.
        assert(calculator.toLocalInterval(localDT("2021-03-07T00:00")) ==
          Some(LocalInterval(localDT("2021-03-02T03:00"), 2.h)))
      }

      "hasPeriodForDay" - {
        "period within a day" in {
          assert(!calculator.hasPeriodForDay(LocalDate.parse("2021-03-01")))
          assert(calculator.hasPeriodForDay(LocalDate.parse("2021-03-02")))
          assert(!calculator.hasPeriodForDay(LocalDate.parse("2021-03-03")))
        }

        "period starts at midnight" in {
          val weekdayPeriod = AdmissionPeriodCalculator(
            WeekdayPeriod(MONDAY, MIDNIGHT, 1.h),
            dateOffset = 0.s)
          assert(!weekdayPeriod.hasPeriodForDay(LocalDate.parse("2021-03-07")))  // Sun
          assert(weekdayPeriod.hasPeriodForDay(LocalDate.parse("2021-03-08")))   // Mon
          assert(!weekdayPeriod.hasPeriodForDay(LocalDate.parse("2021-03-09")))  // Tue
        }

        "period ends at midnight" in {
          val weekdayPeriod = AdmissionPeriodCalculator(
            WeekdayPeriod(MONDAY, LocalTime.of(23, 0), 1.h),
            dateOffset = 0.s)
          assert(!weekdayPeriod.hasPeriodForDay(LocalDate.parse("2021-03-07")))  // Sun
          assert(weekdayPeriod.hasPeriodForDay(LocalDate.parse("2021-03-08")))   // Mon
          assert(!weekdayPeriod.hasPeriodForDay(LocalDate.parse("2021-03-09")))  // Tue
        }

        "period over midnight" in {
          // Saturday 23:00 until Monday 02:00
          val calculator = AdmissionPeriodCalculator(
            WeekdayPeriod(SATURDAY, LocalTime.of(23, 0), (24 + 3).h),
            dateOffset = 0.s)
          assert(!calculator.hasPeriodForDay(LocalDate.parse("2021-03-05")))  // Fri
          assert(calculator.hasPeriodForDay(LocalDate.parse("2021-03-06")))   // Sat
          assert(calculator.hasPeriodForDay(LocalDate.parse("2021-03-07")))   // Sun
          assert(calculator.hasPeriodForDay(LocalDate.parse("2021-03-08")))   // Mon
          assert(!calculator.hasPeriodForDay(LocalDate.parse("2021-03-09")))  // Tue
        }
      }
    }
  }

  "DailyPeriod" - {
    val dateOffset = 6.h
    val dailyPeriod = DailyPeriod(LocalTime.of(8, 0), 2.h)
    val calculator = AdmissionPeriodCalculator(dailyPeriod, dateOffset = dateOffset)

    "calendarStart" in {
      assert(calculator.calendarStart(localDT("2021-10-01T06:00")) == localDT("2021-10-01T06:00"))
      assert(calculator.calendarStart(localDT("2021-10-02T05:59")) == localDT("2021-10-01T06:00"))
    }

    "nextCalendarStart" in {
      assert(calculator.nextCalendarStart(localDT("2021-10-01T06:00")) == localDT("2021-10-02T06:00"))
      assert(calculator.nextCalendarStart(localDT("2021-10-02T05:59")) == localDT("2021-10-02T06:00"))
    }


    "secondOfDay" in {
      assert(dailyPeriod.secondOfDay == 8*3600)
    }

    "secondsSinceStart" in {
      val calculator = new DailyPeriodCalculator(dailyPeriod, dateOffset.toJava)
      assert(calculator.secondsSinceStart(localDT("2021-10-01T00:00")) == -8*3600)
      assert(calculator.secondsSinceStart(localDT("2021-10-01T01:00")) == -7*3600)
      assert(calculator.secondsSinceStart(localDT("2021-10-02T00:00")) == -8*3600)
      assert(calculator.secondsSinceStart(localDT("2021-10-02T08:00")) == 0)
      assert(calculator.secondsSinceStart(localDT("2021-10-02T09:00")) == 3600)
      assert(calculator.secondsSinceStart(localDT("2021-08-29T00:00")) == -8*3600)
    }

    "toInterval" in {
      assert(calculator.toLocalInterval(localDT("2021-10-01T00:00")) ==
        Some(LocalInterval(localDT("2021-10-01T08:00"), 2.h)))
    }

    //"toInterval may return past period" in {
    //  // Past periods will be deleted by the caller. For easier code.
    //  assert(admissionPeriod.toInterval(localDT("2021-10-02T00:00")) ==
    //    Some(LocalInterval(localDT("2021-10-01T08:00"), 2.h)))
    //}

    "hasPeriodForDay" - {
      "period within a day" in {
        assert(calculator.hasPeriodForDay(LocalDate.parse("2021-10-01")))

        val empty = DailyPeriod.checked(1, 0.s)
        assert(empty.isLeft)
      }

      "period starts at midnight" in {
        val dailyPeriod = AdmissionPeriodCalculator(DailyPeriod(MIDNIGHT, 1.h), dateOffset = 0.s)
        assert(dailyPeriod.hasPeriodForDay(LocalDate.parse("2021-08-30")))
      }

      "period ends at midnight" in {
        val dailyPeriod = AdmissionPeriodCalculator(
          DailyPeriod(LocalTime.of(23, 0), 1.h),
          dateOffset = 0.s)
        assert(dailyPeriod.hasPeriodForDay(LocalDate.parse("2021-08-30")))   // Mon
      }

      "period over midnight" in {
        val dailyPeriod = AdmissionPeriodCalculator(
          DailyPeriod(LocalTime.of(23, 0), 24.h),
          dateOffset = 0.s)
        assert(dailyPeriod.hasPeriodForDay(LocalDate.parse("2021-08-28")))   // Sat
      }
    }
  }

  "AlwaysPeriod" - {
    val admissionPeriod = AdmissionPeriodCalculator(AlwaysPeriod, dateOffset = 0.s)

    "toInterval" in {
      assert(AlwaysPeriodCalculator.toLocalInterval(localDT("2021-10-01T00:00")) ==
        Some(LocalInterval.Always))
    }

    "calendarStart (not used)" in {
      assert(admissionPeriod.calendarStart(localDT("2021-10-01T00:00")) ==
        localDT("2021-10-01T00:00"))
      assert(admissionPeriod.calendarStart(localDT("2021-10-01T23:59")) ==
        localDT("2021-10-01T23:59"))
    }

    "nextCalendarStart (not used)" in {
      assert(admissionPeriod.nextCalendarStart(localDT("2021-10-01T00:00")) ==
        localDT("2314-01-10T23:47:16.854775807"))  // 2**31-1 nanoseconds later
    }

    "hasPeriodForDay" in {
      assert(admissionPeriod.hasPeriodForDay(LocalDate.parse("2021-10-01")))
    }
  }

  private def localDT(localDateTime: String) =
    LocalDateTime.parse(localDateTime)
}
