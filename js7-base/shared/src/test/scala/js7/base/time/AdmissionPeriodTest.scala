package js7.base.time

import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichJson}
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class AdmissionPeriodTest extends AnyFreeSpec
{
  "JSON" - {
    "Always" in {
      testJson[AdmissionPeriod](
        AlwaysPeriod,
        json"""
          {
            "TYPE":  "AlwaysPeriod"
          }""")
    }

    "Daily" in {
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
          "duration": -1
        }""".checkedAs[AdmissionPeriod] ==
          Left(Problem(
            "JSON DecodingFailure at : Duration must be positive: DailyPeriod(03:00:00 -1s)")))
    }

    "Weekday" in {
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
            "JSON DecodingFailure at : Invalid WeekdayPeriod duration: WeekdayPeriod(Thursday 03:00:00 -1s)")))
    }

    "MonthlyDatePeriod" in {
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
            "JSON DecodingFailure at : Duration must be positive: MonthlyDatePeriod(270000,-1 seconds)")))
    }

    "MonthlyLastDatePeriod" in {
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
        Left(Problem("JSON DecodingFailure at : Duration must be positive: MonthlyLastDatePeriod(-3600,-1 seconds)")))
    }

    "MonthlyWeekdayPeriod" in {
      testJson[AdmissionPeriod](
        MonthlyWeekdayPeriod(((7+3)*24+3)*3600, 600.s),  // Fourth of month, 3:00, 10 minutes
        json"""
          {
            "TYPE": "MonthlyWeekdayPeriod",
            "secondOfWeek": 874800,
            "duration": 600
          }""")

      assert(json"""
        {
          "TYPE": "MonthlyWeekdayPeriod",
          "secondOfWeek": 874800,
          "duration": -1
        }""".checkedAs[AdmissionPeriod] ==
          Left(Problem("JSON DecodingFailure at : Duration must be positive: MonthlyWeekdayPeriod(874800,-1 seconds)")))
    }
  }
}
