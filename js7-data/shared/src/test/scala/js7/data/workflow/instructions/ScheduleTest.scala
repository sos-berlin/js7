package js7.data.workflow.instructions

import java.time.DayOfWeek.{FRIDAY, MONDAY, SUNDAY}
import java.time.LocalTime
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.time.{AdmissionTimeScheme, DailyPeriod, WeekdayPeriod}
import js7.data.workflow.instructions.Schedule.{Continuous, Periodic, Scheme, Ticking}
import js7.data.workflow.instructions.ScheduleTest._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

final class ScheduleTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(exampleSchedule,
      json"""
      {
        "schemes": [
          {
            "admissionTimeScheme": {
              "periods": [
                {
                  "TYPE": "DailyPeriod",
                  "secondOfDay": 32400,
                  "duration": 4800
                }
              ]
            },
            "repeat": {
              "TYPE": "Periodic",
              "offsets": [ 600, 900, 1200 ],
              "period": 3600
            }
          }, {
            "admissionTimeScheme": {
              "periods": [
                {
                  "TYPE": "WeekdayPeriod",
                  "secondOfWeek": 7200,
                  "duration": 3600
                }, {
                  "TYPE": "WeekdayPeriod",
                  "secondOfWeek": 360000,
                  "duration": 3600
                }
              ]
            },
            "repeat": {
              "TYPE": "Ticking",
              "interval": 1200
            }
          }, {
            "admissionTimeScheme": {
              "periods": [
                {
                  "TYPE": "WeekdayPeriod",
                  "duration": 1800,
                  "secondOfWeek": 583200
                }
              ]
            },
            "repeat": {
              "TYPE": "Continuous",
              "pause": 300
            }
          }, {
            "admissionTimeScheme": {
              "periods": [
                {
                  "TYPE": "WeekdayPeriod",
                  "duration": 1800,
                  "secondOfWeek": 590400
                }
              ]
            },
            "repeat": {
              "TYPE": "Continuous",
              "pause": 60,
              "limit": 3
            }
          }
        ]
      }""")
  }

  "Periodic" in {
    assert(Periodic.checked(1.h, Nil) == Left(Problem("Invalid Periodic arguments")))
    assert(Periodic.checked(1.h, Seq(1.h)) == Left(Problem("Invalid Periodic arguments")))
    assert(Periodic.checked(1.h, Seq(-1.s)) == Left(Problem("Invalid Periodic arguments")))
    assert(Periodic.checked(1.h, Seq(1.s, 1.s)) == Left(Problem("Invalid Periodic arguments")))
  }

  "Ticking" in {
    assert(Ticking.checked(-1.s) == Left(Problem("Invalid Ticking arguments")))
    assert(Ticking.checked(0.s) == Left(Problem("Invalid Ticking arguments")))
    assert(Ticking.checked(1.s) == Right(Ticking(1.s)))
  }

  "Continuous" in {
    assert(Continuous.checked() == Left(Problem("Continuous: limit or pause must be set")))
    assert(Continuous.checked(pause = 0.s) == Left(Problem("Continuous: limit or pause must be set")))
    assert(Continuous.checked(limit = Some(-1)) == Left(Problem("Invalid Continuous arguments")))
    assert(Continuous.checked(pause = -1.s) == Left(Problem("Invalid Continuous arguments")))
    assert(Continuous.checked(pause = 0.s, limit = Some(0)) == Right(Continuous(pause = 0.s, limit = Some(0))))
  }
}

object ScheduleTest
{
  val exampleSchedule = Schedule(Seq(
    Scheme(
      AdmissionTimeScheme(Seq(
        DailyPeriod(localTime("09:00"), 1.h + 20.minutes))),
      Periodic(
        period = 1.h,
        offsets = Seq(10.minute, 15.minute, 20.minute))),

    Scheme(
      AdmissionTimeScheme(Seq(
        WeekdayPeriod(MONDAY, localTime("02:00"), 1.h),
        WeekdayPeriod(FRIDAY, localTime("04:00"), 1.h))),
      Ticking(20.minutes)),

    Scheme(
      AdmissionTimeScheme(Seq(
        WeekdayPeriod(SUNDAY, localTime("18:00"), 30.minutes))),
      Continuous(
        pause = 5.minutes)),

    Scheme(
      AdmissionTimeScheme(Seq(
        WeekdayPeriod(SUNDAY, localTime("20:00"), 30.minutes))),
      Continuous(
        limit = Some(3),
        pause = 1.minute))))

  private def localTime(string: String): LocalTime =
    LocalTime.parse(string)
}
