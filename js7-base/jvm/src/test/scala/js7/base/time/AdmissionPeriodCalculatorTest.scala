package js7.base.time

import java.time.DayOfWeek.{FRIDAY, MONDAY, SATURDAY, SUNDAY, TUESDAY}
import java.time.LocalTime.MIDNIGHT
import java.time.{LocalDate, LocalDateTime, LocalTime, ZoneId}
import js7.base.test.OurTestSuite
import js7.base.time.AdmissionPeriod.WeekSeconds
import js7.base.time.AdmissionPeriodCalculator.{AlwaysPeriodCalculator, DailyPeriodCalculator, MonthlyDatePeriodCalculator, MonthlyLastDatePeriodCalculator, MonthlyLastWeekdayPeriodCalculator, MonthlyWeekdayPeriodCalculator, NoOffset, SpecificDatePeriodCalculator, WeekdayPeriodCalculator, startOfWeek}
import js7.base.time.ScalaTime.*
import scala.jdk.DurationConverters.{JavaDurationOps, ScalaDurationOps}

final class AdmissionPeriodCalculatorTest extends OurTestSuite:

  private given ZoneId = ZoneId.of("Europe/Mariehamn")

  "startOfWeek" in:
    //assert(sinceStartOfWeek(localDT("2022-06-01T00:00")) == localDT("2022-05-30T00:00").toEpochSecond(NoOffset))
    assert(startOfWeek(localDT("2022-06-05T23:59")) == localDT("2022-05-30T00:00").toEpochSecond(NoOffset))
    assert(startOfWeek(localDT("2022-06-06T00:00")) == localDT("2022-06-06T00:00").toEpochSecond(NoOffset))
    assert(startOfWeek(localDT("2022-06-07T00:00")) == localDT("2022-06-06T00:00").toEpochSecond(NoOffset))
    assert(startOfWeek(localDT("2022-06-07T23:59")) == localDT("2022-06-06T00:00").toEpochSecond(NoOffset))
    assert(startOfWeek(localDT("2022-06-08T00:00")) == localDT("2022-06-06T00:00").toEpochSecond(NoOffset))

  "WeekdayPeriod" - {
    val weekdayPeriod = WeekdayPeriod(TUESDAY, LocalTime.of(3, 0), 2.h)
    // 2021-03-01 is a monday

    "No dateOffset" - {
      val calculator = new WeekdayPeriodCalculator(weekdayPeriod, dateOffset = 0.s.toJava)

      "calendarPeriodStart" in:
        import calculator.calendarPeriodStart
        assert(calendarPeriodStart(localDT("2021-03-01T00:00")) == localDT("2021-03-01T00:00"))
        assert(calendarPeriodStart(localDT("2021-03-07T23:59")) == localDT("2021-03-01T00:00"))

      "nextCalendarPeriodStart" in:
        import calculator.nextCalendarPeriodStart
        assert(nextCalendarPeriodStart(localDT("2021-03-01T00:00")) == Some(localDT("2021-03-08T00:00")))
        assert(nextCalendarPeriodStart(localDT("2021-03-07T23:59")) == Some(localDT("2021-03-08T00:00")))

      addWeekdayTests(calculator)
    }

    "dateOffset = 6h" - {
      val calculator = new WeekdayPeriodCalculator(weekdayPeriod, dateOffset = 6.h.toJava)

      "calendarPeriodStart" in:
        import calculator.calendarPeriodStart
        assert(calendarPeriodStart(localDT("2021-03-01T06:00")) == localDT("2021-03-01T06:00"))
        assert(calendarPeriodStart(localDT("2021-03-08T05:59")) == localDT("2021-03-01T06:00"))
        assert(calendarPeriodStart(localDT("2021-03-08T06:00")) == localDT("2021-03-08T06:00"))

      "nextCalendarPeriodStart" in:
        import calculator.nextCalendarPeriodStart
        assert(nextCalendarPeriodStart(localDT("2021-03-01T06:00")) == Some(localDT("2021-03-08T06:00")))
        assert(nextCalendarPeriodStart(localDT("2021-03-07T05:59")) == Some(localDT("2021-03-08T06:00")))

      addWeekdayTests(calculator)
    }

    def addWeekdayTests(calculator: WeekdayPeriodCalculator) =
      // Results are indifferent to dateOffset

      "secondOfWeek (wall clock)" in:
        assert(calculator.admissionPeriod.secondOfWeek == 1*24*3600 + 3*3600)

      "dayNumber, dayName" in:
        assert(calculator.admissionPeriod.dayOfWeek == 1/*monday is 0*/)
        assert(calculator.admissionPeriod.dayName == "Tuesday")

      "admissionPeriodStart" in:
        import calculator.admissionPeriodStart
        assert(admissionPeriodStart(localDT("2022-05-01T00:00")) == localDT("2022-04-26T03:00"))
        assert(admissionPeriodStart(localDT("2022-05-01T01:00")) == localDT("2022-04-26T03:00"))
        assert(admissionPeriodStart(localDT("2022-05-02T00:00")) == localDT("2022-05-03T03:00"))
        assert(admissionPeriodStart(localDT("2022-05-02T03:00")) == localDT("2022-05-03T03:00"))
        assert(admissionPeriodStart(localDT("2022-05-02T04:00")) == localDT("2022-05-03T03:00"))
        assert(admissionPeriodStart(localDT("2022-05-07T00:00")) == localDT("2022-05-03T03:00"))
        assert(admissionPeriodStart(localDT("2022-05-07T23:59")) == localDT("2022-05-03T03:00"))
        assert(admissionPeriodStart(localDT("2022-05-08T00:00")) == localDT("2022-05-03T03:00"))

      "toLocalInterval" in:
        assert(calculator.toLocalInterval(localDT("2021-03-01T00:00")) ==
          Some(LocalInterval(localDT("2021-03-02T03:00"), 2.h)))

      "toLocalInterval may return past period" in:
        // Past periods will be deleted by the caller. For easier code.
        assert(calculator.toLocalInterval(localDT("2021-03-07T00:00")) ==
          Some(LocalInterval(localDT("2021-03-02T03:00"), 2.h)))

      "findIntervals" in:
        val intervals = calculator
          .findLocalIntervals(localDT("2023-07-01T00:00"), until = localDT("2023-08-01T00:00"))
          .toVector
        assert(intervals == Seq(
          LocalInterval(localDT("2023-07-04T03:00"), 2.h),
          LocalInterval(localDT("2023-07-11T03:00"), 2.h),
          LocalInterval(localDT("2023-07-18T03:00"), 2.h),
          LocalInterval(localDT("2023-07-25T03:00"), 2.h)))

      "hasAdmissionPeriodForDay" - {
        "period within a day" in:
          assert(!calculator.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-01")))
          assert(calculator.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-02")))
          assert(!calculator.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-03")))

        "period starts at midnight" in:
          val weekdayPeriod = AdmissionPeriodCalculator(
            WeekdayPeriod(MONDAY, MIDNIGHT, 1.h),
            calculator.dateOffset.toScala)
          assert(!weekdayPeriod.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-07")))  // Sun
          assert(weekdayPeriod.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-08")))   // Mon
          assert(!weekdayPeriod.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-09")))  // Tue

        "period ends at midnight" in:
          val weekdayPeriod = AdmissionPeriodCalculator(
            WeekdayPeriod(MONDAY, LocalTime.of(23, 0), 1.h),
            calculator.dateOffset.toScala)
          assert(!weekdayPeriod.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-07")))  // Sun
          assert(weekdayPeriod.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-08")))   // Mon
          assert(!weekdayPeriod.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-09")))  // Tue

        "period over two midnights" in:
          // Saturday 23:00 until Monday 02:00
          val calc = AdmissionPeriodCalculator(
            WeekdayPeriod(SATURDAY, LocalTime.of(23, 0), (24 + 3).h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-05")))  // Fri
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-06")))   // Sat
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-07")))   // Sun
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-08")))   // Mon
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-09")))  // Tue
      }

      "hasAdmissionPeriodStartForDay" - {
        "period within a day" in:
          assert(!calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-01")))
          assert(calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-02")))
          assert(!calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-03")))

        "period starts at midnight" in:
          val weekdayPeriod = AdmissionPeriodCalculator(
            WeekdayPeriod(MONDAY, MIDNIGHT, 1.h),
            calculator.dateOffset.toScala)
          assert(!weekdayPeriod.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-07")))  // Sun
          assert(weekdayPeriod.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-08")))   // Mon
          assert(!weekdayPeriod.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-09")))  // Tue

        "period over two midnights" in:
          // Saturday 23:00 until Monday 02:00
          val calc = AdmissionPeriodCalculator(
            WeekdayPeriod(SATURDAY, LocalTime.of(23, 0), (24 + 3).h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-05")))  // Fri
          assert(calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-06")))   // Sat
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-07")))   // Sun
      }
  }

  "DailyPeriod" - {
    val dateOffset = 6.h
    val dailyPeriod = DailyPeriod(LocalTime.of(8, 0), 2.h)
    val calculator = AdmissionPeriodCalculator(dailyPeriod, dateOffset = dateOffset)
      .asInstanceOf[DailyPeriodCalculator]

    "calendarPeriodStart" in:
      import calculator.calendarPeriodStart
      assert(calendarPeriodStart(localDT("2021-10-01T06:00")) == localDT("2021-10-01T06:00"))
      assert(calendarPeriodStart(localDT("2021-10-02T05:59")) == localDT("2021-10-01T06:00"))

    "nextCalendarPeriodStart" in:
      import calculator.nextCalendarPeriodStart
      assert(nextCalendarPeriodStart(localDT("2021-10-01T06:00")) == Some(localDT("2021-10-02T06:00")))
      assert(nextCalendarPeriodStart(localDT("2021-10-02T05:59")) == Some(localDT("2021-10-02T06:00")))


    "secondOfDay" in:
      assert(dailyPeriod.secondOfDay == 8*3600)

    "admissionPeriodStart" in:
      val calculator = new DailyPeriodCalculator(dailyPeriod, dateOffset.toJava)
      import calculator.admissionPeriodStart
      assert(admissionPeriodStart(localDT("2021-10-01T00:00")) == localDT("2021-10-01T08:00"))
      assert(admissionPeriodStart(localDT("2021-10-01T01:00")) == localDT("2021-10-01T08:00"))
      assert(admissionPeriodStart(localDT("2021-10-02T00:00")) == localDT("2021-10-02T08:00"))
      assert(admissionPeriodStart(localDT("2021-10-02T08:00")) == localDT("2021-10-02T08:00"))
      assert(admissionPeriodStart(localDT("2021-10-02T09:00")) == localDT("2021-10-02T08:00"))
      assert(admissionPeriodStart(localDT("2021-08-29T00:00")) == localDT("2021-08-29T08:00"))

    "toLocalInterval" in:
      assert(calculator.toLocalInterval(localDT("2021-10-01T00:00")) ==
        Some(LocalInterval(localDT("2021-10-01T08:00"), 2.h)))

    //"toLocalInterval may return past period" in {
    //  // Past periods will be deleted by the caller. For easier code.
    //  assert(admissionPeriod.toLocalInterval(localDT("2021-10-02T00:00")) ==
    //    Some(LocalInterval(localDT("2021-10-01T08:00"), 2.h)))
    //}

    "findIntervals" in:
      val intervals = calculator
        .findLocalIntervals(localDT("2023-07-01T00:00"), until = localDT("2023-08-01T00:00"))
        .toVector
      assert(intervals == Seq(
        LocalInterval(localDT("2023-07-01T08:00"), 2.h),
        LocalInterval(localDT("2023-07-02T08:00"), 2.h),
        LocalInterval(localDT("2023-07-03T08:00"), 2.h),
        LocalInterval(localDT("2023-07-04T08:00"), 2.h),
        LocalInterval(localDT("2023-07-05T08:00"), 2.h),
        LocalInterval(localDT("2023-07-06T08:00"), 2.h),
        LocalInterval(localDT("2023-07-07T08:00"), 2.h),
        LocalInterval(localDT("2023-07-08T08:00"), 2.h),
        LocalInterval(localDT("2023-07-09T08:00"), 2.h),
        LocalInterval(localDT("2023-07-10T08:00"), 2.h),
        LocalInterval(localDT("2023-07-11T08:00"), 2.h),
        LocalInterval(localDT("2023-07-12T08:00"), 2.h),
        LocalInterval(localDT("2023-07-13T08:00"), 2.h),
        LocalInterval(localDT("2023-07-14T08:00"), 2.h),
        LocalInterval(localDT("2023-07-15T08:00"), 2.h),
        LocalInterval(localDT("2023-07-16T08:00"), 2.h),
        LocalInterval(localDT("2023-07-17T08:00"), 2.h),
        LocalInterval(localDT("2023-07-18T08:00"), 2.h),
        LocalInterval(localDT("2023-07-19T08:00"), 2.h),
        LocalInterval(localDT("2023-07-20T08:00"), 2.h),
        LocalInterval(localDT("2023-07-21T08:00"), 2.h),
        LocalInterval(localDT("2023-07-22T08:00"), 2.h),
        LocalInterval(localDT("2023-07-23T08:00"), 2.h),
        LocalInterval(localDT("2023-07-24T08:00"), 2.h),
        LocalInterval(localDT("2023-07-25T08:00"), 2.h),
        LocalInterval(localDT("2023-07-26T08:00"), 2.h),
        LocalInterval(localDT("2023-07-27T08:00"), 2.h),
        LocalInterval(localDT("2023-07-28T08:00"), 2.h),
        LocalInterval(localDT("2023-07-29T08:00"), 2.h),
        LocalInterval(localDT("2023-07-30T08:00"), 2.h),
        LocalInterval(localDT("2023-07-31T08:00"), 2.h)))

    "hasAdmissionPeriodForDay, hasAdmissionPeriodStartForDay" - {
      "period within a day" in:
        assert(calculator.hasAdmissionPeriodForDay(LocalDate.parse("2021-10-01")))
        assert(calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-10-01")))

        val empty = DailyPeriod(1, 0.s).checked
        assert(empty.isLeft)

      "period starts at midnight" in:
        val dailyPeriod = AdmissionPeriodCalculator(DailyPeriod(MIDNIGHT, 1.h), dateOffset = 0.s)
        assert(dailyPeriod.hasAdmissionPeriodForDay(LocalDate.parse("2021-08-30")))
        assert(dailyPeriod.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-08-30")))

      "period ends at midnight" in:
        val dailyPeriod = AdmissionPeriodCalculator(
          DailyPeriod(LocalTime.of(23, 0), 1.h),
          dateOffset = 0.s)
        assert(dailyPeriod.hasAdmissionPeriodForDay(LocalDate.parse("2021-08-30")))   // Mon
        assert(dailyPeriod.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-08-30")))   // Mon

      "period over midnight" in:
        val dailyPeriod = AdmissionPeriodCalculator(
          DailyPeriod(LocalTime.of(23, 0), 24.h),
          dateOffset = 0.s)
        assert(dailyPeriod.hasAdmissionPeriodForDay(LocalDate.parse("2021-08-28")))   // Sat
        assert(dailyPeriod.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-08-28")))   // Sat
    }
  }

  "AlwaysPeriod" - {
    val admissionPeriod = AdmissionPeriodCalculator(AlwaysPeriod, dateOffset = 0.s)
      .asInstanceOf[AlwaysPeriodCalculator.type]

    "toLocalInterval" in:
      assert(AlwaysPeriodCalculator.toLocalInterval(localDT("2021-10-01T00:00")) ==
        Some(LocalInterval.Always))

    "findIntervals" in:
      val intervals = AlwaysPeriodCalculator
        .findLocalIntervals(localDT("2023-07-01T00:00"), until = localDT("2023-08-01T00:00"))
        .toVector
      assert(intervals == Seq(LocalInterval.Always))

    "nextCalendarPeriodStart (not used)" in:
      assert(admissionPeriod.nextCalendarPeriodStart(localDT("2021-10-01T00:00")) ==
        None)

    "hasAdmissionPeriodForDay" in:
      assert(admissionPeriod.hasAdmissionPeriodForDay(LocalDate.parse("2021-10-01")))
  }

  "MonthlyDatePeriod" - {
    val period = MonthlyDatePeriod(2, LocalTime.parse("03:00"), 1.h)

    "secondOfMonth" in:
      assert(period.secondOfMonth == 1 * 24*3600 + 3*3600)

    "No dateOffset" - {
      val calculator = new MonthlyDatePeriodCalculator(period, dateOffset = 0.s.toJava)
      import calculator.{calendarPeriodStart, nextCalendarPeriodStart}

      "calendarPeriodStart" in:
        assert(calendarPeriodStart(localDT("2021-02-28T23:59")) == localDT("2021-02-01T00:00"))
        assert(calendarPeriodStart(localDT("2021-03-01T00:00")) == localDT("2021-03-01T00:00"))
        assert(calendarPeriodStart(localDT("2021-03-31T23:59")) == localDT("2021-03-01T00:00"))
        assert(calendarPeriodStart(localDT("2021-04-01T00:00")) == localDT("2021-04-01T00:00"))

      "nextCalendarPeriodStart" in:
        assert(nextCalendarPeriodStart(localDT("2021-03-01T00:00")) == Some(localDT("2021-04-01T00:00")))
        assert(nextCalendarPeriodStart(localDT("2021-03-31T23:59")) == Some(localDT("2021-04-01T00:00")))

      addMonthlyTests(calculator)
    }

    "dateOffset = 6h" - {
      val calculator = new MonthlyDatePeriodCalculator(period, dateOffset = 6.h.toJava)
      import calculator.{calendarPeriodStart, nextCalendarPeriodStart}

      "calendarStart" in:
        assert(calendarPeriodStart(localDT("2021-03-01T06:00")) == localDT("2021-03-01T06:00"))
        assert(calendarPeriodStart(localDT("2021-03-08T05:59")) == localDT("2021-03-01T06:00"))

      "nextCalendarPeriodStart" in:
        assert(nextCalendarPeriodStart(localDT("2021-03-01T06:00")) == Some(localDT("2021-04-01T06:00")))
        assert(nextCalendarPeriodStart(localDT("2021-03-31T05:59")) == Some(localDT("2021-04-01T06:00")))

      addMonthlyTests(calculator)
    }

    def addMonthlyTests(calculator: MonthlyDatePeriodCalculator) =
      import calculator.dateOffset
      // Results are indifferent to dateOffset

      "admissionPeriodStart" in:
        import calculator.admissionPeriodStart
        assert(admissionPeriodStart(localDT("2021-03-01T00:00")) == localDT("2021-03-02T03:00"))
        assert(admissionPeriodStart(localDT("2021-03-01T01:00")) == localDT("2021-03-02T03:00"))
        assert(admissionPeriodStart(localDT("2021-03-02T00:00")) == localDT("2021-03-02T03:00"))
        assert(admissionPeriodStart(localDT("2021-03-02T03:00")) == localDT("2021-03-02T03:00"))
        assert(admissionPeriodStart(localDT("2021-03-02T04:00")) == localDT("2021-03-02T03:00"))
        assert(admissionPeriodStart(localDT("2021-03-31T00:00")) == localDT("2021-03-02T03:00"))
        assert(admissionPeriodStart(localDT("2021-03-31T23:59")) == localDT("2021-03-02T03:00"))
        assert(admissionPeriodStart(localDT("2021-04-01T00:00")) == localDT("2021-04-02T03:00"))

      "toLocalInterval" in:
        assert(calculator.toLocalInterval(localDT("2021-03-01T00:00")) ==
          Some(LocalInterval(localDT("2021-03-02T03:00"), 1.h)))

      "toLocalInterval may return past period" in:
        // Past periods will be deleted by the caller. For easier code.
        assert(calculator.toLocalInterval(localDT("2021-03-07T00:00")) ==
          Some(LocalInterval(localDT("2021-03-02T03:00"), 1.h)))

      "findIntervals" in:
        val intervals = calculator
          .findLocalIntervals(localDT("2023-07-01T00:00"), until = localDT("2023-08-01T00:00"))
          .toVector
        assert(intervals == Seq(
          LocalInterval(localDT("2023-07-02T03:00"), 1.h)))

      "hasAdmissionPeriodForDay" - {
        "period within a day" in:
          assert(!calculator.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-01")))
          assert(calculator.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-02")))
          assert(!calculator.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-03")))

        "period starts at midnight" in:
          val calc = AdmissionPeriodCalculator(
            MonthlyDatePeriod(2, LocalTime.parse("00:00"), 1.h),
            dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-01")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-02")))
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-03")))

        "period ends at midnight" in:
          val calc = AdmissionPeriodCalculator(
            MonthlyDatePeriod(2, LocalTime.parse("03:00"), 1.h),
            dateOffset = 0.s)
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-01")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-02")))
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-03")))

        "period over two midnights" in:
          // 3rd of month 23:00 until 5th of month 02:00
          val calc = AdmissionPeriodCalculator(
            MonthlyDatePeriod(3, LocalTime.parse("23:00"), (24 + 3).h),
            dateOffset.toScala)
          assert(calc.admissionPeriod == MonthlyDatePeriod((2*24+23)*3600, 27.h))

          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-02")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-03")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-04")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-05")))
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-06")))
      }

      "hasAdmissionPeriodStartForDay" - {
        "period within a day" in:
          assert(!calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-01")))
          assert(calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-02")))
          assert(!calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-03")))

        "period starts at midnight" in:
          val calc = AdmissionPeriodCalculator(
            MonthlyDatePeriod(2, LocalTime.parse("00:00"), 1.h),
            dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-01")))
          assert(calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-02")))
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-03")))

        "period over two midnights" in:
          // 3rd of month 23:00 until 5th of month 02:00
          val calc = AdmissionPeriodCalculator(
            MonthlyDatePeriod(3, LocalTime.parse("23:00"), (24 + 3).h),
            dateOffset.toScala)
          assert(calc.admissionPeriod == MonthlyDatePeriod((2*24+23)*3600, 27.h))

          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-02")))
          assert(calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-03")))
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-04")))
      }

    "Use last day of month if month does not have the requested day" in:
      val period = MonthlyDatePeriod(31, LocalTime.parse("03:00"), 1.h)
      val calc = AdmissionPeriodCalculator(period, 0.h).asInstanceOf[MonthlyDatePeriodCalculator]
      assert(calc.admissionPeriodStart(localDT("2022-02-01T00:00")) == localDT("2022-02-28T03:00"))
      assert(calc.admissionPeriodStart(localDT("2022-03-01T00:00")) == localDT("2022-03-31T03:00"))
      assert(calc.admissionPeriodStart(localDT("2022-04-01T00:00")) == localDT("2022-04-30T03:00"))

      assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-02-28")))
      assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-03-30")))
      assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-03-31")))
      assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-04-30")))

      assert(calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-02-28")))
      assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-03-30")))
      assert(calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-03-31")))
      assert(calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-04-30")))
  }

  "MonthlyLastDatePeriod" - {
    val period = MonthlyLastDatePeriod(-2, LocalTime.parse("03:00"), 1.h)

    "lastSecondOfMonth" in:
      assert(period.lastSecondOfMonth == -(2*24-3)*3600)

    "No dateOffset" - {
      val calculator = new MonthlyLastDatePeriodCalculator(period, dateOffset = 0.s.toJava)
      import calculator.{calendarPeriodStart, nextCalendarPeriodStart}

      "calendarPeriodStart" in:
        assert(calendarPeriodStart(localDT("2021-03-01T00:00")) == localDT("2021-03-01T00:00"))
        assert(calendarPeriodStart(localDT("2021-03-31T23:59")) == localDT("2021-03-01T00:00"))

      "nextCalendarPeriodStart" in:
        assert(nextCalendarPeriodStart(localDT("2021-03-01T00:00")) == Some(localDT("2021-04-01T00:00")))
        assert(nextCalendarPeriodStart(localDT("2021-03-31T23:59")) == Some(localDT("2021-04-01T00:00")))

      addMonthlyTests(calculator)
    }

    "dateOffset = 6h" - {
      val calculator = new MonthlyLastDatePeriodCalculator(period, dateOffset = 6.h.toJava)
      import calculator.{calendarPeriodStart, nextCalendarPeriodStart}

      "calendarStart" in:
        assert(calendarPeriodStart(localDT("2021-03-01T06:00")) == localDT("2021-03-01T06:00"))
        assert(calendarPeriodStart(localDT("2021-03-08T05:59")) == localDT("2021-03-01T06:00"))

      "nextCalendarPeriodStart" in:
        assert(nextCalendarPeriodStart(localDT("2021-03-01T06:00")) == Some(localDT("2021-04-01T06:00")))
        assert(nextCalendarPeriodStart(localDT("2021-03-31T05:59")) == Some(localDT("2021-04-01T06:00")))

      addMonthlyTests(calculator)
    }

    def addMonthlyTests(calculator: MonthlyLastDatePeriodCalculator) =
      // Results are indifferent to dateOffset

      "admissionPeriodStart" in:
        import calculator.admissionPeriodStart
        assert(admissionPeriodStart(localDT("2022-06-01T00:00")) == localDT("2022-06-29T03:00"))
        assert(admissionPeriodStart(localDT("2022-06-01T01:00")) == localDT("2022-06-29T03:00"))
        assert(admissionPeriodStart(localDT("2022-06-30T03:00")) == localDT("2022-06-29T03:00"))
        assert(admissionPeriodStart(localDT("2022-06-30T23:59")) == localDT("2022-06-29T03:00"))
        assert(admissionPeriodStart(localDT("2022-07-01T00:00")) == localDT("2022-07-30T03:00"))

      "toLocalInterval" in:
        assert(calculator.toLocalInterval(localDT("2021-03-01T00:00")) ==
          Some(LocalInterval(localDT("2021-03-30T03:00"), 1.h)))

      "toLocalInterval may return past period" in:
        // Past periods will be deleted by the caller. For easier code.
        assert(calculator.toLocalInterval(localDT("2021-03-07T00:00")) ==
          Some(LocalInterval(localDT("2021-03-30T03:00"), 1.h)))

      "findIntervals" in:
        val intervals = calculator
          .findLocalIntervals(localDT("2023-07-01T00:00"), until = localDT("2023-08-01T00:00"))
          .toList
        assert(intervals == Seq(
          LocalInterval(localDT("2023-07-30T03:00"), 1.h)))

      "hasAdmissionPeriodForDay" - {
        "period within a day" in:
          assert(!calculator.hasAdmissionPeriodForDay(LocalDate.parse("2021-06-01")))
          assert(!calculator.hasAdmissionPeriodForDay(LocalDate.parse("2021-06-28")))
          assert(calculator.hasAdmissionPeriodForDay(LocalDate.parse("2021-06-29")))
          assert(!calculator.hasAdmissionPeriodForDay(LocalDate.parse("2021-06-30")))

        "period starts at midnight" in:
          val calc = AdmissionPeriodCalculator(
            MonthlyLastDatePeriod(-2, LocalTime.parse("00:00"), 1.h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-29")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-30")))
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-31")))

        "period ends at midnight" in:
          val calc = AdmissionPeriodCalculator(
            MonthlyLastDatePeriod(-2, LocalTime.parse("23:00"), 1.h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-29")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-30")))
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-31")))

        "period over two midnights" in:
          val calc = AdmissionPeriodCalculator(
            MonthlyLastDatePeriod(-3, LocalTime.parse("23:00:00"), (24 + 3).h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-28")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-29")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-30")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-03-31")))
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2021-04-01")))
      }

      "hasAdmissionPeriodStartForDay" - {
        "period within a day" in:
          assert(!calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-06-01")))
          assert(!calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-06-28")))
          assert(calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-06-29")))
          assert(!calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-06-30")))

        "period starts at midnight" in:
          val calc = AdmissionPeriodCalculator(
            MonthlyLastDatePeriod(-2, LocalTime.parse("00:00"), 1.h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-29")))
          assert(calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-30")))
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-31")))

        "period over two midnights" in:
          val calc = AdmissionPeriodCalculator(
            MonthlyLastDatePeriod(-3, LocalTime.parse("23:00:00"), (24 + 3).h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-28")))
          assert(calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-29")))
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2021-03-30")))
      }
  }

  "MonthlyWeekdayPeriod" - {
    val period = MonthlyWeekdayPeriod(2, FRIDAY, LocalTime.of(3, 0), 1.h)
    // For example 2022-06-10

    locally:
      val period1 = MonthlyWeekdayPeriod(1, FRIDAY, LocalTime.of(3, 0), 1.h)
      val period4 = MonthlyWeekdayPeriod(4, FRIDAY, LocalTime.of(3, 0), 1.h)

      "secondOfWeeks" in:
        assert(period.secondOfWeeks == ((7 + 4)*24 + 3)*3600)
        assert(period1.secondOfWeeks == (4*24 + 3)*3600)
        assert(period4.secondOfWeeks == ((3*7 + 4)*24 + 3)*3600)

      "secondOfDay" in:
        assert(period.secondOfDay == 3*3600)
        assert(period1.secondOfDay == 3*3600)
        assert(period4.secondOfDay == 3*3600)

      "dayOfWeek" in:
        assert(period.dayOfWeek == 4)
        assert(period1.dayOfWeek == 4)
        assert(period4.dayOfWeek == 4)

      "shiftWeeks" in:
        assert(period.shiftWeeks == 1)
        assert(period1.shiftWeeks == 0)
        assert(period4.shiftWeeks == 3)

    "No dateOffset" - {
      val calculator = new MonthlyWeekdayPeriodCalculator(period, dateOffset = 0.s.toJava)
      import calculator.{calendarPeriodStart, nextCalendarPeriodStart}

      "calendarPeriodStart" in:
        assert(calendarPeriodStart(localDT("2022-06-01T00:00")) == localDT("2022-06-01T00:00"))
        assert(calendarPeriodStart(localDT("2022-06-30T23:59")) == localDT("2022-06-01T00:00"))

      "nextCalendarPeriodStart" in:
        assert(nextCalendarPeriodStart(localDT("2022-06-01T00:00")) == Some(localDT("2022-07-01T00:00")))
        assert(nextCalendarPeriodStart(localDT("2022-06-30T23:59")) == Some(localDT("2022-07-01T00:00")))

      addMonthlyTests(calculator)
    }

    "dateOffset = 6h" - {
      val calculator = new MonthlyWeekdayPeriodCalculator(period, dateOffset = 6.h.toJava)
      import calculator.{calendarPeriodStart, nextCalendarPeriodStart}

      "calendarStart" in:
        assert(calendarPeriodStart(localDT("2022-06-01T06:00")) == localDT("2022-06-01T06:00"))
        assert(calendarPeriodStart(localDT("2022-07-01T05:59")) == localDT("2022-06-01T06:00"))
        assert(calendarPeriodStart(localDT("2022-07-01T06:00")) == localDT("2022-07-01T06:00"))

      "nextCalendarPeriodStart" in:
        assert(nextCalendarPeriodStart(localDT("2022-06-01T06:00")) == Some(localDT("2022-07-01T06:00")))
        assert(nextCalendarPeriodStart(localDT("2022-06-30T05:59")) == Some(localDT("2022-07-01T06:00")))

      addMonthlyTests(calculator)
    }

    def addMonthlyTests(calculator: MonthlyWeekdayPeriodCalculator) =
      // Results are indifferent to dateOffset

      "admissionPeriodStart" in:
        import calculator.admissionPeriodStart

        assert(admissionPeriodStart(localDT("2022-07-01T00:00")) == localDT("2022-07-08T03:00"))
        assert(admissionPeriodStart(localDT("2022-12-01T00:00")) == localDT("2022-12-09T03:00"))
        assert(admissionPeriodStart(localDT("2022-06-01T00:00")) == localDT("2022-06-10T03:00"))
        assert(admissionPeriodStart(localDT("2022-02-01T00:00")) == localDT("2022-02-11T03:00"))
        assert(admissionPeriodStart(localDT("2022-08-01T00:00")) == localDT("2022-08-12T03:00"))
        assert(admissionPeriodStart(localDT("2022-05-01T00:00")) == localDT("2022-05-13T03:00"))
        assert(admissionPeriodStart(localDT("2022-10-01T00:00")) == localDT("2022-10-14T03:00"))

      "toLocalInterval" in:
        assert(calculator.toLocalInterval(localDT("2022-07-01T00:00")) ==
          Some(LocalInterval(localDT("2022-07-08T03:00"), 1.h)))

      "toLocalInterval may return past period" in:
        // Past periods will be deleted by the caller. For easier code.
        assert(calculator.toLocalInterval(localDT("2022-06-30T00:00")) ==
          Some(LocalInterval(localDT("2022-06-10T03:00"), 1.h)))

      "findIntervals" in:
        val intervals = calculator
          .findLocalIntervals(localDT("2023-07-01T00:00"), until = localDT("2023-08-01T00:00"))
          .toVector
        assert(intervals == Seq(
          LocalInterval(localDT("2023-07-14T03:00"), 1.h)))

      "hasAdmissionPeriodForDay" - {
        "period within a day" in:
          assert(!calculator.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-01")))
          assert(!calculator.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-09")))
          assert(calculator.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-10")))
          assert(!calculator.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-11")))

        "period starts at midnight" in:
          val calc = AdmissionPeriodCalculator(
            MonthlyWeekdayPeriod(2, FRIDAY, LocalTime.of(0, 0), 1.h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-09")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-10")))
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-11")))

        "period ends at midnight" in:
          val calc = AdmissionPeriodCalculator(
            MonthlyWeekdayPeriod(2, FRIDAY, LocalTime.of(23, 0), 1.h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-09")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-10")))
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-11")))

        "period over two midnights" in:
          val calc = AdmissionPeriodCalculator(
            MonthlyWeekdayPeriod(2, FRIDAY, LocalTime.of(23, 0), (24+3).h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-09")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-10")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-11")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-12")))
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-13")))
      }

      "hasAdmissionPeriodStartForDay" - {
        "period within a day" in:
          assert(!calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-06-01")))
          assert(!calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-06-09")))
          assert(calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-06-10")))
          assert(!calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-06-11")))

        "period starts at midnight" in:
          val calc = AdmissionPeriodCalculator(
            MonthlyWeekdayPeriod(2, FRIDAY, LocalTime.of(0, 0), 1.h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-06-09")))
          assert(calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-06-10")))
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-06-11")))

        "period over two midnights" in:
          val calc = AdmissionPeriodCalculator(
            MonthlyWeekdayPeriod(2, FRIDAY, LocalTime.of(23, 0), (24+3).h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-06-09")))
          assert(calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-06-10")))
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-06-11")))
      }
  }

  "MonthlyLastWeekdayPeriod" - {
    // Each friday of the second last week
    val period = MonthlyLastWeekdayPeriod(-2, FRIDAY, LocalTime.of(3, 0), 1.h)
    // For example 2022-06-17

    locally:
      val period1 = MonthlyLastWeekdayPeriod(-1, FRIDAY, LocalTime.of(3, 0), 1.h)
      val period4 = MonthlyLastWeekdayPeriod(-4, FRIDAY, LocalTime.of(3, 0), 1.h)

      "secondOfWeeks" in:
        assert(period.secondOfWeeks == (4*24 + 3)*3600 - 2*WeekSeconds)
        assert(period1.secondOfWeeks == (4*24 + 3)*3600 - 1*WeekSeconds)
        assert(period4.secondOfWeeks == (4*24 + 3)*3600 - 4*WeekSeconds)

        assert(period == MonthlyLastWeekdayPeriod((4*24 + 3)*3600 - 2*WeekSeconds, 1.h))
        assert(period1 == MonthlyLastWeekdayPeriod((4*24 + 3)*3600 - 1*WeekSeconds, 1.h))
        assert(period4 == MonthlyLastWeekdayPeriod((4*24 + 3)*3600 - 4*WeekSeconds, 1.h))

        // Last second of last Sunday:
        assert(MonthlyLastWeekdayPeriod(-1, SUNDAY, LocalTime.of(23, 59, 59), 1.h) ==
          MonthlyLastWeekdayPeriod(-1, 1.h))

      "secondOfDay" in:
        assert(period.secondOfDay == 3*3600)
        assert(period1.secondOfDay == 3*3600)
        assert(period4.secondOfDay == 3*3600)

      "dayOfWeek" in:
        assert(period.dayOfWeek == 4)
        assert(period1.dayOfWeek == 4)
        assert(period4.dayOfWeek == 4)

      "shiftWeeks" in:
        assert(period.shiftWeeks == -1)
        assert(period1.shiftWeeks == 0)
        assert(period4.shiftWeeks == -3)

        assert(MonthlyLastWeekdayPeriod(-1, MONDAY, LocalTime.MIDNIGHT, 1.h).shiftWeeks == 0)
        assert(MonthlyLastWeekdayPeriod(-1, SUNDAY, LocalTime.of(0, 0, 1), 1.h).shiftWeeks == 0)


    "No dateOffset" - {
      val calculator = new MonthlyLastWeekdayPeriodCalculator(period, dateOffset = 0.s.toJava)
      import calculator.{calendarPeriodStart, nextCalendarPeriodStart}

      "calendarPeriodStart" in:
        assert(calendarPeriodStart(localDT("2022-06-01T00:00")) == localDT("2022-06-01T00:00"))
        assert(calendarPeriodStart(localDT("2022-06-30T23:59")) == localDT("2022-06-01T00:00"))

      "nextCalendarPeriodStart" in:
        assert(nextCalendarPeriodStart(localDT("2022-06-01T00:00")) == Some(localDT("2022-07-01T00:00")))
        assert(nextCalendarPeriodStart(localDT("2022-06-30T23:59")) == Some(localDT("2022-07-01T00:00")))

      addMonthlyTests(calculator)
    }

    "dateOffset = 6h" - {
      val calculator = new MonthlyLastWeekdayPeriodCalculator(period, dateOffset = 6.h.toJava)
      import calculator.{calendarPeriodStart, nextCalendarPeriodStart}

      "calendarStart" in:
        assert(calendarPeriodStart(localDT("2022-06-01T06:00")) == localDT("2022-06-01T06:00"))
        assert(calendarPeriodStart(localDT("2022-07-01T05:59")) == localDT("2022-06-01T06:00"))
        assert(calendarPeriodStart(localDT("2022-07-01T06:00")) == localDT("2022-07-01T06:00"))

      "nextCalendarPeriodStart" in:
        assert(nextCalendarPeriodStart(localDT("2022-06-01T06:00")) == Some(localDT("2022-07-01T06:00")))
        assert(nextCalendarPeriodStart(localDT("2022-06-30T05:59")) == Some(localDT("2022-07-01T06:00")))

      addMonthlyTests(calculator)
    }

    def addMonthlyTests(calculator: MonthlyLastWeekdayPeriodCalculator) =
      // Results are indifferent to dateOffset

      "admissionPeriodStart" in:
        import calculator.admissionPeriodStart
                                                                                                 // 1st last
        assert(admissionPeriodStart(localDT("2022-06-01T00:00")) == localDT("2022-06-17T03:00")) // mon thu
        assert(admissionPeriodStart(localDT("2022-02-01T00:00")) == localDT("2022-02-18T03:00")) // tue mon
        assert(admissionPeriodStart(localDT("2022-08-01T00:00")) == localDT("2022-08-19T03:00")) // mon wed
        assert(admissionPeriodStart(localDT("2021-08-01T00:00")) == localDT("2021-08-20T03:00")) // sun tue
        assert(admissionPeriodStart(localDT("2022-10-01T00:00")) == localDT("2022-10-21T03:00")) // sun mon
        assert(admissionPeriodStart(localDT("2022-07-01T00:00")) == localDT("2022-07-22T03:00")) // fri sun
        assert(admissionPeriodStart(localDT("2022-12-01T00:00")) == localDT("2022-12-23T03:00")) // thu sat
        assert(admissionPeriodStart(localDT("2022-01-01T00:00")) == localDT("2022-01-21T03:00")) // sun mon
        assert(admissionPeriodStart(localDT("2022-09-01T00:00")) == localDT("2022-09-23T03:00")) // thu fri

      "toLocalInterval" in:
        assert(calculator.toLocalInterval(localDT("2022-07-01T00:00")) ==
          Some(LocalInterval(localDT("2022-07-22T03:00"), 1.h)))

      "toLocalInterval may return past period" in:
        // Past periods will be deleted by the caller. For easier code.
        assert(calculator.toLocalInterval(localDT("2022-06-30T00:00")) ==
          Some(LocalInterval(localDT("2022-06-17T03:00"), 1.h)))

      "findIntervals" in:
        val intervals = calculator
          .findLocalIntervals(localDT("2023-07-01T00:00"), until = localDT("2023-08-01T00:00"))
          .toVector
        assert(intervals == Seq(
          LocalInterval(localDT("2023-07-21T03:00"), 1.h)))

      "hasAdmissionPeriodForDay" - {
        "period within a day" in:
          assert(!calculator.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-01")))
          assert(!calculator.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-06")))
          assert(calculator.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-17")))
          assert(!calculator.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-18")))

        "period starts at midnight" in:
          val calc = AdmissionPeriodCalculator(
            MonthlyLastWeekdayPeriod(-2, FRIDAY, LocalTime.of(0, 0), 1.h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-16")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-17")))
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-18")))

        "period ends at midnight" in:
          val calc = AdmissionPeriodCalculator(
            MonthlyLastWeekdayPeriod(-2, FRIDAY, LocalTime.of(23, 0), 1.h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-16")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-17")))
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-18")))

        "period over two midnights" in:
          val calc = AdmissionPeriodCalculator(
            MonthlyLastWeekdayPeriod(-2, FRIDAY, LocalTime.of(23, 0), (24+3).h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-16")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-17")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-18")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-19")))
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-06-20")))
      }

      "hasAdmissionPeriodStartForDay" - {
        "period within a day" in:
          assert(!calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-06-01")))
          assert(!calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-06-06")))
          assert(calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-06-17")))
          assert(!calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-06-18")))

        "period starts at midnight" in:
          val calc = AdmissionPeriodCalculator(
            MonthlyLastWeekdayPeriod(-2, FRIDAY, LocalTime.of(0, 0), 1.h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-06-16")))
          assert(calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-06-17")))
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-06-18")))

        "period over two midnights" in:
          val calc = AdmissionPeriodCalculator(
            MonthlyLastWeekdayPeriod(-2, FRIDAY, LocalTime.of(23, 0), (24+3).h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-06-16")))
          assert(calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-06-17")))
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-06-18")))
      }
  }

  "SpecificDatePeriod" - {
    val period = SpecificDatePeriod(localDT("2022-09-28T12:00"), 1.h)

    "No dateOffset" - {
      val calculator = new SpecificDatePeriodCalculator(period, dateOffset = 0.s.toJava)

      "calendarPeriodStart" in:
        import calculator.calendarPeriodStart
        assert(calendarPeriodStart(localDT("2000-01-01T00:00")) == localDT("2022-09-28T12:00"))
        assert(calendarPeriodStart(localDT("2022-09-28T11:59")) == localDT("2022-09-28T12:00"))
        assert(calendarPeriodStart(localDT("2022-09-28T12:00")) == localDT("2022-09-28T12:00"))
        assert(calendarPeriodStart(localDT("2022-09-28T12:01")) == localDT("2022-09-28T12:00"))
        assert(calendarPeriodStart(localDT("3333-03-03T03:03")) == localDT("2022-09-28T12:00"))

      "nextCalendarPeriodStart" in:
        import calculator.nextCalendarPeriodStart
        assert(nextCalendarPeriodStart(localDT("2000-01-01T00:00")) == Some(localDT("2022-09-28T12:00")))
        assert(nextCalendarPeriodStart(localDT("2022-09-28T11:59")) == Some(localDT("2022-09-28T12:00")))
        assert(nextCalendarPeriodStart(localDT("2022-09-28T12:00")) == Some(localDT("2022-09-28T12:00")))
        assert(nextCalendarPeriodStart(localDT("2022-09-28T12:01")) == None)
        assert(nextCalendarPeriodStart(localDT("3333-03-03T03:03")) == None)

      addDateOffsetIndifferentTests(calculator)
    }

    "dateOffset = 6h" - {
      val calculator = new SpecificDatePeriodCalculator(period, dateOffset = 6.h.toJava)

      "calendarPeriodStart" in:
        import calculator.calendarPeriodStart
        assert(calendarPeriodStart(localDT("2000-01-01T00:00")) == localDT("2022-09-28T18:00"))
        assert(calendarPeriodStart(localDT("2022-09-28T17:59")) == localDT("2022-09-28T18:00"))
        assert(calendarPeriodStart(localDT("2022-09-28T18:00")) == localDT("2022-09-28T18:00"))
        assert(calendarPeriodStart(localDT("2022-09-28T18:01")) == localDT("2022-09-28T18:00"))
        assert(calendarPeriodStart(localDT("3333-03-03T03:03")) == localDT("2022-09-28T18:00"))

      "nextCalendarPeriodStart" in:
        import calculator.nextCalendarPeriodStart
        assert(nextCalendarPeriodStart(localDT("2000-01-01T00:00")) == Some(localDT("2022-09-28T18:00")))
        assert(nextCalendarPeriodStart(localDT("2022-09-28T17:59")) == Some(localDT("2022-09-28T18:00")))
        assert(nextCalendarPeriodStart(localDT("2022-09-28T18:00")) == Some(localDT("2022-09-28T18:00")))
        assert(nextCalendarPeriodStart(localDT("2022-09-28T18:01")) == None)
        assert(nextCalendarPeriodStart(localDT("3333-03-03T03:03")) == None)

      addDateOffsetIndifferentTests(calculator)
    }

    def addDateOffsetIndifferentTests(calculator: SpecificDatePeriodCalculator) =
      // Results are indifferent to dateOffset
      val specificDate = localDT("2022-09-28T12:00")

      "admissionPeriodStart" in:
        import calculator.admissionPeriodStart
        assert(admissionPeriodStart(localDT("2000-01-01T00:00")) == specificDate)
        assert(admissionPeriodStart(localDT("2022-09-28T17:59")) == specificDate)
        assert(admissionPeriodStart(localDT("2022-09-28T18:00")) == specificDate)
        assert(admissionPeriodStart(localDT("2022-09-28T18:01")) == specificDate)
        assert(admissionPeriodStart(localDT("3333-03-03T03:03")) == specificDate)

      "toLocalInterval" in:
        assert(calculator.toLocalInterval(localDT("2000-01-01T00:00")) ==
          Some(LocalInterval(specificDate, 1.h)))
        assert(calculator.toLocalInterval(localDT("2022-09-28T00:00")) ==
          Some(LocalInterval(specificDate, 1.h)))

      "toLocalInterval may return past period" in:
        // Past periods will be deleted by the caller. For easier code.
        assert(calculator.toLocalInterval(localDT("2222-02-02T00:00")) ==
          Some(LocalInterval(specificDate, 1.h)))

      "findIntervals" in:
        val intervals = calculator
          .findLocalIntervals(localDT("2022-09-01T00:00"), until = localDT("2022-10-01T00:00"))
          .toVector
        assert(intervals == Seq(
          LocalInterval(localDT("2022-09-28T12:00"), 1.h)))

      "findIntervals other month" in:
        val intervals = calculator
          .findLocalIntervals(localDT("2023-07-01T00:00"), until = localDT("2023-08-01T00:00"))
          .toVector
        assert(intervals.isEmpty)

      "hasAdmissionPeriodForDay" - {
        "period within a day" in:
          assert(!calculator.hasAdmissionPeriodForDay(LocalDate.parse("2022-09-27")))
          assert(calculator.hasAdmissionPeriodForDay(LocalDate.parse("2022-09-28")))
          assert(!calculator.hasAdmissionPeriodForDay(LocalDate.parse("2022-09-29")))

        "period starts at midnight" in:
          val calc = AdmissionPeriodCalculator(
            SpecificDatePeriod(localDT("2022-09-28T00:00"), 1.h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-09-27")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-09-28")))
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-09-29")))

        "period ends at midnight" in:
          val calc = AdmissionPeriodCalculator(
            SpecificDatePeriod(localDT("2022-09-28T23:00"), 1.h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-09-27")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-09-28")))
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-09-29")))

        "period over two midnights" in:
          val calc = AdmissionPeriodCalculator(
            SpecificDatePeriod(localDT("2022-09-27T23:00"), 24.h + 2.h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-09-26")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-09-27")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-09-28")))
          assert(calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-09-29")))
          assert(!calc.hasAdmissionPeriodForDay(LocalDate.parse("2022-09-30")))
      }

      "hasAdmissionPeriodStartForDay" - {
        "period within a day" in:
          assert(!calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-09-27")))
          assert(calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-09-28")))
          assert(!calculator.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-09-29")))

        "period starts at midnight" in:
          val calc = AdmissionPeriodCalculator(
            SpecificDatePeriod(localDT("2022-09-28T00:00"), 1.h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-09-27")))
          assert(calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-09-28")))
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-09-29")))

        "period over two midnights" in:
          val calc = AdmissionPeriodCalculator(
            SpecificDatePeriod(localDT("2022-09-27T23:00"), 24.h + 2.h),
            calculator.dateOffset.toScala)
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-09-26")))
          assert(calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-09-27")))
          assert(!calc.hasAdmissionPeriodStartForDay(LocalDate.parse("2022-09-28")))
      }
  }

  private def localDT(localDateTime: String) =
    LocalDateTime.parse(localDateTime)
