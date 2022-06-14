package js7.base.time

import java.time.DayOfWeek.{MONDAY, SATURDAY, TUESDAY}
import java.time.LocalTime.MIDNIGHT
import java.time.{LocalDate, LocalDateTime, LocalTime}
import js7.base.time.AdmissionPeriodCalculator.{AlwaysPeriodCalculator, DailyPeriodCalculator, NoOffset, WeekdayPeriodCalculator, startOfWeek}
import js7.base.time.ScalaTime._
import org.scalatest.freespec.AnyFreeSpec
import scala.jdk.DurationConverters.{JavaDurationOps, ScalaDurationOps}

final class AdmissionPeriodCalculatorTest extends AnyFreeSpec
{
  "startOfWeek" in {
    //assert(sinceStartOfWeek(localDT("2022-06-01T00:00")) == localDT("2022-05-30T00:00").toEpochSecond(NoOffset))
    assert(startOfWeek(localDT("2022-06-05T23:59")) == localDT("2022-05-30T00:00").toEpochSecond(NoOffset))
    assert(startOfWeek(localDT("2022-06-06T00:00")) == localDT("2022-06-06T00:00").toEpochSecond(NoOffset))
    assert(startOfWeek(localDT("2022-06-07T00:00")) == localDT("2022-06-06T00:00").toEpochSecond(NoOffset))
    assert(startOfWeek(localDT("2022-06-07T23:59")) == localDT("2022-06-06T00:00").toEpochSecond(NoOffset))
    assert(startOfWeek(localDT("2022-06-08T00:00")) == localDT("2022-06-06T00:00").toEpochSecond(NoOffset))
  }

  "WeekdayPeriod" - {
    val weekdayPeriod = WeekdayPeriod(TUESDAY, LocalTime.of(3, 0), 2.h)
    // 2021-03-01 is a monday

    "No dateOffset" - {
      val calculator = new WeekdayPeriodCalculator(weekdayPeriod, dateOffset = 0.s.toJava)

      "calendarPeriodStart" in {
        import calculator.calendarPeriodStart
        assert(calendarPeriodStart(localDT("2021-03-01T00:00")) == localDT("2021-03-01T00:00"))
        assert(calendarPeriodStart(localDT("2021-03-07T23:59")) == localDT("2021-03-01T00:00"))
      }

      "nextCalendarPeriodStart" in {
        import calculator.nextCalendarPeriodStart
        assert(nextCalendarPeriodStart(localDT("2021-03-01T00:00")) == Some(localDT("2021-03-08T00:00")))
        assert(nextCalendarPeriodStart(localDT("2021-03-07T23:59")) == Some(localDT("2021-03-08T00:00")))
      }

      addWeekdayTests(calculator)
    }

    "dateOffset = 6h" - {
      val calculator = new WeekdayPeriodCalculator(weekdayPeriod, dateOffset = 6.h.toJava)

      "calendarPeriodStart" in {
        import calculator.calendarPeriodStart
        assert(calendarPeriodStart(localDT("2021-03-01T06:00")) == localDT("2021-03-01T06:00"))
        assert(calendarPeriodStart(localDT("2021-03-08T05:59")) == localDT("2021-03-01T06:00"))
        assert(calendarPeriodStart(localDT("2021-03-08T06:00")) == localDT("2021-03-08T06:00"))
      }

      "nextCalendarPeriodStart" in {
        import calculator.nextCalendarPeriodStart
        assert(nextCalendarPeriodStart(localDT("2021-03-01T06:00")) == Some(localDT("2021-03-08T06:00")))
        assert(nextCalendarPeriodStart(localDT("2021-03-07T05:59")) == Some(localDT("2021-03-08T06:00")))
      }

      addWeekdayTests(calculator)
    }

    def addWeekdayTests(calculator: WeekdayPeriodCalculator) = {
      // Results are indifferent to datePeriod

      "secondOfWeek (wall clock)" in {
        assert(calculator.admissionPeriod.secondOfWeek == 1*24*3600 + 3*3600)
      }

      "dayNumber, dayName" in {
        assert(calculator.admissionPeriod.dayOffset == 1/*monday is 0*/)
        assert(calculator.admissionPeriod.dayName == "Tuesday")
      }

      "admissionPeriodStart" in {
        import calculator.admissionPeriodStart
        assert(admissionPeriodStart(localDT("2022-05-01T00:00")) == localDT("2022-04-26T03:00"))
        assert(admissionPeriodStart(localDT("2022-05-01T01:00")) == localDT("2022-04-26T03:00"))
        assert(admissionPeriodStart(localDT("2022-05-02T00:00")) == localDT("2022-05-03T03:00"))
        assert(admissionPeriodStart(localDT("2022-05-02T03:00")) == localDT("2022-05-03T03:00"))
        assert(admissionPeriodStart(localDT("2022-05-02T04:00")) == localDT("2022-05-03T03:00"))
        assert(admissionPeriodStart(localDT("2022-05-07T00:00")) == localDT("2022-05-03T03:00"))
        assert(admissionPeriodStart(localDT("2022-05-07T23:59")) == localDT("2022-05-03T03:00"))
        assert(admissionPeriodStart(localDT("2022-05-08T00:00")) == localDT("2022-05-03T03:00"))
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
            calculator.dateOffset.toScala)
          assert(!weekdayPeriod.hasPeriodForDay(LocalDate.parse("2021-03-07")))  // Sun
          assert(weekdayPeriod.hasPeriodForDay(LocalDate.parse("2021-03-08")))   // Mon
          assert(!weekdayPeriod.hasPeriodForDay(LocalDate.parse("2021-03-09")))  // Tue
        }

        "period ends at midnight" in {
          val weekdayPeriod = AdmissionPeriodCalculator(
            WeekdayPeriod(MONDAY, LocalTime.of(23, 0), 1.h),
            calculator.dateOffset.toScala)
          assert(!weekdayPeriod.hasPeriodForDay(LocalDate.parse("2021-03-07")))  // Sun
          assert(weekdayPeriod.hasPeriodForDay(LocalDate.parse("2021-03-08")))   // Mon
          assert(!weekdayPeriod.hasPeriodForDay(LocalDate.parse("2021-03-09")))  // Tue
        }

        "period over two midnights" in {
          // Saturday 23:00 until Monday 02:00
          val calc = AdmissionPeriodCalculator(
            WeekdayPeriod(SATURDAY, LocalTime.of(23, 0), (24 + 3).h),
            calculator.dateOffset.toScala)
          assert(!calc.hasPeriodForDay(LocalDate.parse("2021-03-05")))  // Fri
          assert(calc.hasPeriodForDay(LocalDate.parse("2021-03-06")))   // Sat
          assert(calc.hasPeriodForDay(LocalDate.parse("2021-03-07")))   // Sun
          assert(calc.hasPeriodForDay(LocalDate.parse("2021-03-08")))   // Mon
          assert(!calc.hasPeriodForDay(LocalDate.parse("2021-03-09")))  // Tue
        }
      }
    }
  }

  "DailyPeriod" - {
    val dateOffset = 6.h
    val dailyPeriod = DailyPeriod(LocalTime.of(8, 0), 2.h)
    val calculator = AdmissionPeriodCalculator(dailyPeriod, dateOffset = dateOffset)
      .asInstanceOf[DailyPeriodCalculator]

    "calendarPeriodStart" in {
      import calculator.calendarPeriodStart
      assert(calendarPeriodStart(localDT("2021-10-01T06:00")) == localDT("2021-10-01T06:00"))
      assert(calendarPeriodStart(localDT("2021-10-02T05:59")) == localDT("2021-10-01T06:00"))
    }

    "nextCalendarPeriodStart" in {
      import calculator.nextCalendarPeriodStart
      assert(nextCalendarPeriodStart(localDT("2021-10-01T06:00")) == Some(localDT("2021-10-02T06:00")))
      assert(nextCalendarPeriodStart(localDT("2021-10-02T05:59")) == Some(localDT("2021-10-02T06:00")))
    }


    "secondOfDay" in {
      assert(dailyPeriod.secondOfDay == 8*3600)
    }

    "admissionPeriodStart" in {
      val calculator = new DailyPeriodCalculator(dailyPeriod, dateOffset.toJava)
      import calculator.admissionPeriodStart
      assert(admissionPeriodStart(localDT("2021-10-01T00:00")) == localDT("2021-10-01T08:00"))
      assert(admissionPeriodStart(localDT("2021-10-01T01:00")) == localDT("2021-10-01T08:00"))
      assert(admissionPeriodStart(localDT("2021-10-02T00:00")) == localDT("2021-10-02T08:00"))
      assert(admissionPeriodStart(localDT("2021-10-02T08:00")) == localDT("2021-10-02T08:00"))
      assert(admissionPeriodStart(localDT("2021-10-02T09:00")) == localDT("2021-10-02T08:00"))
      assert(admissionPeriodStart(localDT("2021-08-29T00:00")) == localDT("2021-08-29T08:00"))
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

        val empty = DailyPeriod(1, 0.s).checked
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
      .asInstanceOf[AlwaysPeriodCalculator.type]

    "toInterval" in {
      assert(AlwaysPeriodCalculator.toLocalInterval(localDT("2021-10-01T00:00")) ==
        Some(LocalInterval.Always))
    }

    "nextCalendarPeriodStart (not used)" in {
      assert(admissionPeriod.nextCalendarPeriodStart(localDT("2021-10-01T00:00")) ==
        None)
    }

    "hasPeriodForDay" in {
      assert(admissionPeriod.hasPeriodForDay(LocalDate.parse("2021-10-01")))
    }
  }

  private def localDT(localDateTime: String) =
    LocalDateTime.parse(localDateTime)
}
