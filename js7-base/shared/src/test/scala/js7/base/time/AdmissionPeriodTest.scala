package js7.base.time

import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichJson}
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.tester.CirceJsonTester.testJson

final class AdmissionPeriodTest extends OurTestSuite:

  "JSON" - {
    "Always" in:
      testJson[AdmissionPeriod](
        AlwaysPeriod,
        json"""
          {
            "TYPE":  "AlwaysPeriod"
          }""")

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
            "JSON DecodingFailure at : Duration must be positive and not longer than 24 hours: DailyPeriod(daily at 03:00, 0s)")))

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
            "JSON DecodingFailure at : Invalid WeekdayPeriod duration: WeekdayPeriod(weekly at Thursday 03:00, -1s)")))

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
