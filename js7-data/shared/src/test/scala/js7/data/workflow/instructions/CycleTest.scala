package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils._
import js7.data.workflow.instructions.CycleTest._
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.{Instruction, Workflow}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class CycleTest extends AnyFreeSpec
{
  "JSON" in {
    testJson[Instruction](exampleCycle,
      json"""
      {
        "TYPE": "Cycle",
        "schedule": {
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
        },
        "cycleWorkflow": {
          "instructions": []
        }
      }""")
  }
}

object CycleTest
{
  val exampleCycle = Cycle(
    ScheduleTest.exampleSchedule,
    Workflow.empty)
}
