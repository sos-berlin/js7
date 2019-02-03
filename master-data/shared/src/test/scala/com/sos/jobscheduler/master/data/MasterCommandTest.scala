package com.sos.jobscheduler.master.data

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.crypt.PgpSignature
import com.sos.jobscheduler.data.filebased.{SignedRepoObject, VersionId}
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.WorkflowPath
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
      assert(Batch(List(NoOperation, EmergencyStop, NoOperation)).toString == "Batch(NoOperation, EmergencyStop, NoOperation)")
      assert(Batch(List(NoOperation, EmergencyStop, NoOperation, NoOperation)).toString == "Batch(NoOperation, EmergencyStop, 2×NoOperation)")
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

  "CancelOrder" - {
    "CancelOrder NotStarted" in {
      testJson[MasterCommand](CancelOrder(OrderId("ORDER"), CancelMode.NotStarted),
        json"""{
          "TYPE": "CancelOrder",
          "orderId": "ORDER"
        }""")
    }

    "CancelOrder FreshOrStarted" in {
      testJson[MasterCommand](CancelOrder(OrderId("ORDER"), CancelMode.FreshOrStarted),
        json"""{
          "TYPE": "CancelOrder",
          "orderId": "ORDER",
          "mode": {
            "TYPE": "FreshOrStarted"
          }
        }""")
    }
  }

  "UpdateRepo" in {
    testJson[MasterCommand](UpdateRepo(
      VersionId("1"),
      change = SignedRepoObject(
        message = """{"TYPE": "Workflow", ...}""",
        signature = PgpSignature(
           """-----BEGIN PGP SIGNATURE-----
            |
            |...
            |-----END PGP SIGNATURE-----
            |""".stripMargin)) :: Nil,
      delete = WorkflowPath("/WORKFLOW-A") :: AgentPath("/AGENT-A") :: Nil),
      json"""{
        "TYPE": "UpdateRepo",
        "versionId": "1",
        "change": [
          {
            "message": "{\"TYPE\": \"Workflow\", ...}",
            "signature": {
              "TYPE": "PGP",
              "string": "-----BEGIN PGP SIGNATURE-----\n\n...\n-----END PGP SIGNATURE-----\n"
            }
          }
        ],
        "delete": [
          {
            "TYPE": "WorkflowPath",
            "path": "/WORKFLOW-A"
          }, {
            "TYPE": "AgentPath",
            "path": "/AGENT-A"
          }
        ]
      }""")
  }

  "ReplaceRepo" in {
    testJson[MasterCommand](ReplaceRepo(
      VersionId("1"),
      objects = SignedRepoObject(
        message = """{"TYPE": "Workflow", ...}""",
        signature = PgpSignature(
          """|-----BEGIN PGP SIGNATURE-----
            |
            |...
            |-----END PGP SIGNATURE-----
            |""".stripMargin)) :: Nil),
      json"""{
        "TYPE": "ReplaceRepo",
        "versionId": "1",
        "objects": [
          {
            "message": "{\"TYPE\": \"Workflow\", ...}",
            "signature": {
              "TYPE": "PGP",
              "string": "-----BEGIN PGP SIGNATURE-----\n\n...\n-----END PGP SIGNATURE-----\n"
            }
          }
        ]
      }""")
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

  "IssueTestEvent" in {  // For tests only
    testJson[MasterCommand](IssueTestEvent,
      json"""{
        "TYPE": "IssueTestEvent"
      }""")
  }

  "ScheduleOrdersEvery" in {
    testJson[MasterCommand](
      ScheduleOrdersEvery(12345.millis),
      json"""{
        "TYPE": "ScheduleOrdersEvery",
        "every": 12.345
       }""")
  }

  "TakeSnapshot" in {
    testJson[MasterCommand](TakeSnapshot,
      json"""{
        "TYPE": "TakeSnapshot"
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
