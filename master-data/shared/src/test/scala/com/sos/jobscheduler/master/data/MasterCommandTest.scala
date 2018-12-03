package com.sos.jobscheduler.master.data

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.master.data.MasterCommand._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec
import scala.concurrent.duration._

final class MasterCommandTest extends FreeSpec {

  "Batch" - {
    "Batch" in {
      testJson[MasterCommand](Batch(List(NoOperation, EmergencyStop)),
        json"""{
          "TYPE": "Batch",
          "commands": [
            { "TYPE": "NoOperation" },
            { "TYPE": "EmergencyStop" }
          ]
        }""")
    }

    "Batch.toString" in {
      assert(Batch(List(NoOperation, EmergencyStop, NoOperation)).toString == "Batch(3 commands: NoOperation, EmergencyStop, NoOperation)")
      assert(Batch(List(NoOperation, EmergencyStop, NoOperation, NoOperation)).toString == "Batch(4 commands: NoOperation, EmergencyStop, NoOperation, ...)")
    }

    "BatchResponse" in {
      testJson[MasterCommand.Response](Batch.Response(Valid(Response.Accepted) :: Invalid(Problem("PROBLEM")) :: Nil),
        json"""{
          "TYPE": "BatchResponse",
          "responses": [
            {
              "TYPE": "Accepted"
            }, {
              "TYPE": "Problem",
              "message": "PROBLEM"
            }
          ]
        }""")
    }

    "BatchResponse.toString" in {
      val threeResponses = Valid(Response.Accepted) :: Invalid(Problem("PROBLEM")) :: Valid(Response.Accepted) :: Nil
      assert(Batch.Response(threeResponses).toString == "BatchResponse(2 succeeded and 1 failed)")
      assert(Batch.Response(threeResponses ::: Valid(Response.Accepted) :: Nil).toString == "BatchResponse(3 succeeded and 1 failed)")
    }
  }

  "EmergencyStop" in {
    testJson[MasterCommand](EmergencyStop,
      json"""{
        "TYPE": "EmergencyStop"
      }""")
  }

  "KeepEvents" in {
    testJson[MasterCommand](
      KeepEvents(123),
      json"""{
        "TYPE": "KeepEvents",
        "after": 123
      }""")
  }

  "NoOperation" in {
    testJson[MasterCommand](NoOperation,
      json"""{
        "TYPE": "NoOperation"
      }""")
  }

  "ReadConfigurationDirectory" - {
    "with versionId" in {
      testJson[MasterCommand](ReadConfigurationDirectory(Some(VersionId("VERSION"))),
        json"""{
          "TYPE": "ReadConfigurationDirectory",
          "versionId": "VERSION"
        }""")
    }

    "without versionId" in {
      testJson[MasterCommand](ReadConfigurationDirectory(None),
        json"""{
          "TYPE": "ReadConfigurationDirectory"
        }""")
    }
  }

  "ScheduleOrdersEvery" in {
    testJson[MasterCommand](
      ScheduleOrdersEvery(12345.millis),
      json"""{
        "TYPE": "ScheduleOrdersEvery",
        "every": 12.345
       }""")
  }

  "Terminate" in {
    testJson[MasterCommand](Terminate,
      json"""{
        "TYPE": "Terminate"
      }""")
  }

  "Response.Accepted" in {
    testJson[MasterCommand.Response](
      Response.Accepted,
      json"""{
        "TYPE": "Accepted"
      }""")
  }
}
