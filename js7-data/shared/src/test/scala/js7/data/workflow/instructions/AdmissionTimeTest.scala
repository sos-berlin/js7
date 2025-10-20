package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.time.{AdmissionTimeScheme, DailyPeriod}
import js7.data.workflow.{Instruction, Workflow}
import js7.tester.CirceJsonTester.testJson
import scala.concurrent.duration.*

final class AdmissionTimeTest extends OurTestSuite:

  "JSON" in:
    import Instructions.jsonCodec
    testJson[Instruction](
      AdmissionTime(AdmissionTimeScheme(
        Seq(DailyPeriod(3600, 1.minute))),
        skipIfNoAdmissionStartForOrderDay = true):
        Workflow.empty,
      json"""{
        "TYPE": "AdmissionTime",
        "admissionTimeScheme": {
          "periods": [
            {
              "TYPE": "DailyPeriod",
                "duration": 60,
                "secondOfDay": 3600
            }
          ]
        },
        "skipIfNoAdmissionStartForOrderDay": true,
        "block": {
          "instructions": []
        }
      }""")
