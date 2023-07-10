package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.data.execution.workflow.instructions.ScheduleTester
import js7.data.workflow.instructions.CycleTest.*
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.{Instruction, Workflow}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class CycleTest extends OurTestSuite
{
  "JSON" in {
    testJsonDecoder[Instruction](
      Cycle(Schedule(Nil), Workflow.empty),
      json"""
      {
        "TYPE": "Cycle",
        "schedule": {
          "schemes": []
        },
        "cycleWorkflow": {
          "instructions": []
        }
      }""")

    testJson[Instruction](exampleCycle,
      json"""
      {
        "TYPE": "Cycle",
        "onlyOnePeriod": false,
        "schedule": {
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
        },
        "cycleWorkflow": {
          "instructions": []
        }
      }""")
  }
}

object CycleTest
{
  val exampleCycle = Cycle(ScheduleTester.schedule, Workflow.empty)
}
