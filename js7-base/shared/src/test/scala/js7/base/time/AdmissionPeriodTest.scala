package js7.base.time

import java.time.DayOfWeek.{MONDAY, SUNDAY}
import java.time.LocalTime
import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichJson}
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.Problems.PeriodCrossesProductionDayBoundaryProblem
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.tester.CirceJsonTester.testJson
import scala.concurrent.duration.*

final class AdmissionPeriodTest extends OurTestSuite:

  "JSON" - {
    "DailyPlan" in:
      testJson[AdmissionPeriod](
        DailyPeriod(3*3600, 600.s), // 3:00, 10 minutes
        json"""
          {
            "TYPE": "DailyPeriod",
            "secondOfDay": 10800,
            "duration": 600
          }""")

      assert(json"""
        {
          "TYPE": "DailyPeriod",
          "secondOfDay": 10800,
          "duration": 0
        }""".checkedAs[AdmissionPeriod] ==
          Left(Problem(
            "JSON DecodingFailure at : Duration must be positive: DailyPeriod(03:00, 0s)")))

    "Weekday" in:
      testJson[AdmissionPeriod](
        WeekdayPeriod((3*24+3)*3600, 600.s),  // Thursday, 3:00, 10 minutes
        json"""
          {
            "TYPE": "WeekdayPeriod",
            "secondOfWeek": 270000,
            "duration": 600
          }""")

      assert(json"""
        {
          "TYPE": "WeekdayPeriod",
          "secondOfWeek": 270000,
          "duration": -1
        }""".checkedAs[AdmissionPeriod] ==
          Left(Problem(
            "JSON DecodingFailure at : Invalid WeekdayPeriod duration: WeekdayPeriod(Thursday 03:00, -1s)")))

    "MonthlyDatePeriod" in:
      testJson[AdmissionPeriod](
        MonthlyDatePeriod((3*24+3)*3600, 600.s),  // Fourth of month, 3:00, 10 minutes
        json"""
          {
            "TYPE": "MonthlyDatePeriod",
            "secondOfMonth": 270000,
            "duration": 600
          }""")

        assert(json"""
          {
            "TYPE": "MonthlyDatePeriod",
            "secondOfMonth": 270000,
            "duration": -1
          }""".checkedAs[AdmissionPeriod] ==
          Left(Problem(
            "JSON DecodingFailure at : Duration must be positive: MonthlyDatePeriod(4th of month, 03:00, -1s)")))

    "MonthlyLastDatePeriod" in:
      testJson[AdmissionPeriod](
        MonthlyLastDatePeriod(-3600, 600.s),  // Last day of month, 23:00
        json"""
          {
            "TYPE": "MonthlyLastDatePeriod",
            "lastSecondOfMonth": -3600,
            "duration": 600
          }""")

      assert(json"""
        {
          "TYPE": "MonthlyLastDatePeriod",
          "lastSecondOfMonth": -3600,
          "duration": -1
        }""".checkedAs[AdmissionPeriod] ==
        Left(Problem("JSON DecodingFailure at : Duration must be positive: MonthlyLastDatePeriod(last day of month, 01:00, -1s)")))

    "MonthlyWeekdayPeriod" in:
      testJson[AdmissionPeriod](
        MonthlyWeekdayPeriod(((7+3)*24+3)*3600, 600.s),  // Second thursday, 3:00, 10 minutes
        json"""
          {
            "TYPE": "MonthlyWeekdayPeriod",
            "secondOfWeeks": 874800,
            "duration": 600
          }""")

      assert(json"""
        {
          "TYPE": "MonthlyWeekdayPeriod",
          "secondOfWeeks": 874800,
          "duration": -1
        }""".checkedAs[AdmissionPeriod] ==
          Left(Problem("JSON DecodingFailure at : Duration must be positive: MonthlyWeekdayPeriod(2nd Thursday 03:00, -1s)")))

    "MonthlyLastWeekdayPeriod" in:
      testJson[AdmissionPeriod](
        MonthlyLastWeekdayPeriod((4*24 + 3)*3600 - (2*7*24*3600), 600.s),  // Second last week, friday, 3:00, 10 minutes
        json"""
          {
            "TYPE": "MonthlyLastWeekdayPeriod",
            "secondOfWeeks": -853200,
            "duration": 600
          }""")

      assert(json"""
        {
          "TYPE": "MonthlyLastWeekdayPeriod",
          "secondOfWeeks": -853200,
          "duration": -1
        }""".checkedAs[AdmissionPeriod] ==
          Left(Problem("JSON DecodingFailure at : Duration must be positive: MonthlyLastWeekdayPeriod(2nd last Friday 03:00, -1s)")))

    "SpecificDatePeriod" in:
      val epochSeconds = 1664366400
      assert(epochSeconds == ts"2022-09-28T12:00:00Z".toEpochSecond)
      testJson[AdmissionPeriod](
        SpecificDatePeriod(epochSeconds, 600.s),
        json"""
          {
            "TYPE": "SpecificDatePeriod",
            "secondsSinceLocalEpoch": 1664366400,
            "duration": 600
          }""")

      assert(SpecificDatePeriod(epochSeconds, 600.s).toString == "SpecificDatePeriod(2022-09-28T12:00:00, 10min)")
  }

  "check(dateOffset)" - {
    "DailyPeriod" in:
      // Near the calendar day before
      assert(DailyPeriod(LocalTime.parse("00:00"), 2.h).check(dateOffset = 0.s) == Nil)
      assert(DailyPeriod(LocalTime.parse("00:00"), 2.h).check(dateOffset = 2.h) == Nil)
      assert(DailyPeriod(LocalTime.parse("00:00"), 3.h).check(dateOffset = 2.h) == List(
        PeriodCrossesProductionDayBoundaryProblem(DailyPeriod(LocalTime.parse("00:00"), 3.h), 2.h)))

      // Production day = calendar day
      assert(DailyPeriod(LocalTime.parse("03:00"), 3.h).check(dateOffset = 2.h) == Nil)
      assert(DailyPeriod(LocalTime.parse("04:00"), 3.h).check(dateOffset = 2.h) == Nil)

      // Near the next calendar day
      assert(DailyPeriod(LocalTime.parse("22:00"), 4.h).check(dateOffset = 2.h) == Nil)
      assert(DailyPeriod(LocalTime.parse("22:00"), 5.h).check(dateOffset = 2.h) == List(
        PeriodCrossesProductionDayBoundaryProblem(DailyPeriod(LocalTime.parse("22:00"), 5.h), 2.h)))

    "WeekdayPeriod" in:
      // Near the calendar day before
      assert(WeekdayPeriod(MONDAY, LocalTime.parse("00:00"), 2.h).check(dateOffset = 0.s) == Nil)
      assert(WeekdayPeriod(MONDAY, LocalTime.parse("00:00"), 2.h).check(dateOffset = 2.h) == Nil)
      assert(WeekdayPeriod(MONDAY, LocalTime.parse("00:00"), 3.h).check(dateOffset = 2.h) == List(
        PeriodCrossesProductionDayBoundaryProblem(WeekdayPeriod(MONDAY, LocalTime.parse("00:00"), 3.h), 2.h)))

      // Production day = calendar day
      assert(WeekdayPeriod(MONDAY, LocalTime.parse("03:00"), 3.h).check(dateOffset = 2.h) == Nil)
      assert(WeekdayPeriod(MONDAY, LocalTime.parse("04:00"), 3.h).check(dateOffset = 2.h) == Nil)

      // Near the next calendar day
      assert(WeekdayPeriod(SUNDAY, LocalTime.parse("22:00"), 4.h).check(dateOffset = 2.h) == Nil)
      assert(WeekdayPeriod(SUNDAY, LocalTime.parse("22:00"), 5.h).check(dateOffset = 2.h) == List(
        PeriodCrossesProductionDayBoundaryProblem(WeekdayPeriod(SUNDAY, LocalTime.parse("22:00"), 5.h), 2.h)))

    "MonthlyDatePeriod" in:
      // Near the calendar day before
      assert(MonthlyDatePeriod(1, LocalTime.parse("00:00"), 2.h).check(dateOffset = 0.s) == Nil)
      assert(MonthlyDatePeriod(1, LocalTime.parse("00:00"), 2.h).check(dateOffset = 2.h) == Nil)
      assert(MonthlyDatePeriod(1, LocalTime.parse("00:00"), 3.h).check(dateOffset = 2.h) == List(
        PeriodCrossesProductionDayBoundaryProblem(MonthlyDatePeriod(1, LocalTime.parse("00:00"), 3.h), 2.h)))

      // Production day = calendar day
      assert(MonthlyDatePeriod(1, LocalTime.parse("03:00"), 3.h).check(dateOffset = 2.h) == Nil)
      assert(MonthlyDatePeriod(1, LocalTime.parse("04:00"), 3.h).check(dateOffset = 2.h) == Nil)

      // Near the next calendar day
      assert(MonthlyDatePeriod(30, LocalTime.parse("22:00"), 5.h).check(dateOffset = 2.h) == Nil)
      assert(MonthlyDatePeriod(30, LocalTime.parse("22:00"), 1.day + 4.h).check(dateOffset = 2.h) == Nil)
      assert(MonthlyDatePeriod(30, LocalTime.parse("22:00"), 1.day + 5.h).check(dateOffset = 2.h) == List(
        PeriodCrossesProductionDayBoundaryProblem(MonthlyDatePeriod(30, LocalTime.parse("22:00"), 1.day + 5.h), 2.h)))

      assert(MonthlyDatePeriod(31, LocalTime.parse("22:00"), 4.h).check(dateOffset = 2.h) == Nil)
      assert(MonthlyDatePeriod(31, LocalTime.parse("22:00"), 5.h).check(dateOffset = 2.h) == List(
        PeriodCrossesProductionDayBoundaryProblem(MonthlyDatePeriod(31, LocalTime.parse("22:00"), 5.h), 2.h)))

    "MonthlyLastDatePeriod" in:
      // 28-day month is always assumed: a period start -28.days through 0.days before end of month.
      assert(MonthlyLastDatePeriod(-28, LocalTime.parse("00:00"), 2.h).lastSecondOfMonth == -28 * 24 * 3600)
      assert(MonthlyLastDatePeriod(-28, LocalTime.parse("01:00"), 2.h).lastSecondOfMonth == (-28 * 24 + 1) * 3600)

      // Near the calendar day before
      assert(MonthlyLastDatePeriod(-28, LocalTime.parse("00:00"), 2.h).check(dateOffset = 0.s) == Nil)
      assert(MonthlyLastDatePeriod(-28, LocalTime.parse("00:00"), 2.h).check(dateOffset = 2.h) == Nil)
      assert(MonthlyLastDatePeriod(-28, LocalTime.parse("00:00"), 3.h).check(dateOffset = 2.h) == Nil)

      // Production day = calendar day
      assert(MonthlyLastDatePeriod(-28, LocalTime.parse("03:00"), 3.h).check(dateOffset = 2.h) == Nil)
      assert(MonthlyLastDatePeriod(-28, LocalTime.parse("04:00"), 3.h).check(dateOffset = 2.h) == Nil)

      assert(MonthlyLastDatePeriod(-10, LocalTime.parse("03:00"), 3.h).check(dateOffset = 2.h) == Nil)
      assert(MonthlyLastDatePeriod(-10, LocalTime.parse("04:00"), 3.h).check(dateOffset = 2.h) == Nil)

      // Near the next calendar day
      assert(MonthlyLastDatePeriod(-1, LocalTime.parse("22:00"), 4.h).check(dateOffset = 2.h) == Nil)
      assert(MonthlyLastDatePeriod(-1, LocalTime.parse("22:00"), 5.h).check(dateOffset = 2.h) == List(
        PeriodCrossesProductionDayBoundaryProblem(MonthlyLastDatePeriod(-1, LocalTime.parse("22:00"), 5.h), 2.h)))

      assert(MonthlyLastDatePeriod(-2, LocalTime.parse("22:00"), 1.day + 4.h).check(dateOffset = 2.h) == Nil)
      assert(MonthlyLastDatePeriod(-2, LocalTime.parse("22:00"), 1.day + 5.h).check(dateOffset = 2.h) == List(
        PeriodCrossesProductionDayBoundaryProblem(MonthlyLastDatePeriod(-2, LocalTime.parse("22:00"), 1.day + 5.h), 2.h)))
  }
