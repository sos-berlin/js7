package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.data.execution.workflow.instructions.ScheduleTester
import js7.data.workflow.instructions.Schedule.{Continuous, Periodic, Ticking}
import js7.tester.CirceJsonTester.testJson

final class ScheduleTest extends OurTestSuite:
  "JSON" in:
    testJson(ScheduleTester.schedule,
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
                }, {
                  "TYPE": "DailyPeriod",
                  "duration": 960,
                  "secondOfDay": 43200
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
                  "secondOfWeek": 93600,
                  "duration": 3600
                }, {
                  "TYPE": "WeekdayPeriod",
                  "secondOfWeek": 446400,
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
                  "secondOfWeek": 590400
                }
              ]
            },
            "repeat": {
              "TYPE": "Continuous",
              "pause": 60,
              "limit": 3
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
          }
        ]
      }""")

  "Periodic" in:
    assert(Periodic.checked(1.h, Nil) == Left(Problem("Invalid Periodic arguments")))
    assert(Periodic.checked(1.h, Seq(1.h)) == Left(Problem("Invalid Periodic arguments")))
    assert(Periodic.checked(1.h, Seq(-1.s)) == Left(Problem("Invalid Periodic arguments")))
    assert(Periodic.checked(1.h, Seq(1.s, 1.s)) == Left(Problem("Invalid Periodic arguments")))

  "Ticking" in:
    assert(Ticking.checked(-1.s) == Left(Problem("Invalid Ticking arguments")))
    assert(Ticking.checked(0.s) == Left(Problem("Invalid Ticking arguments")))
    assert(Ticking.checked(1.s) == Right(Ticking(1.s)))

  "Continuous" in:
    assert(Continuous.checked() == Left(Problem("Continuous: limit or pause must be set")))
    assert(Continuous.checked(pause = 0.s) == Left(Problem("Continuous: limit or pause must be set")))
    assert(Continuous.checked(limit = Some(-1)) == Left(Problem("Invalid Continuous arguments")))
    assert(Continuous.checked(pause = -1.s) == Left(Problem("Invalid Continuous arguments")))
    assert(Continuous.checked(pause = 0.s, limit = Some(0)) == Right(Continuous(pause = 0.s, limit = Some(0))))
