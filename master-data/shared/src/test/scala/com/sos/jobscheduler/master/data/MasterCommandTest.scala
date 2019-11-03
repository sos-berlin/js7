package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.cluster.ClusterNodeId
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.crypt.{GenericSignature, SignedString}
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.data.MasterCommand._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

final class MasterCommandTest extends FreeSpec
{
  "AppointBackupNode" in {
    testJson[MasterCommand](AppointBackupNode(ClusterNodeId("NODE-ID"), Uri("http://example.com")),
      json"""{
        "TYPE": "AppointBackupNode",
        "nodeId": "NODE-ID",
        "uri": "http://example.com"
      }""")
  }

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
      testJson[MasterCommand.Response](Batch.Response(Right(Response.Accepted) :: Left(Problem("PROBLEM")) :: Nil),
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
      val threeResponses = Right(Response.Accepted) :: Left(Problem("PROBLEM")) :: Right(Response.Accepted) :: Nil
      assert(Batch.Response(threeResponses).toString == "BatchResponse(2 succeeded and 1 failed)")
      assert(Batch.Response(threeResponses ::: Right(Response.Accepted) :: Nil).toString == "BatchResponse(3 succeeded and 1 failed)")
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

  "PassiveNodeFollows" in {
    testJson[MasterCommand](
      PassiveNodeFollows(ClusterNodeId("NODE-ID"), Uri("http://example.com")),
      json"""{
        "TYPE": "PassiveNodeFollows",
        "passiveNodeId": "NODE-ID",
        "activeUri": "http://example.com"
      }""")
  }

  "PassiveNodeFollowsResponse" in {
    testJson[MasterCommand.Response](
      PassiveNodeFollows.Response(EventId(123)),
      json"""{
        "TYPE": "PassiveNodeFollowsResponse",
        "eventId": 123
      }""")
  }

  "ReplaceRepo" in {
    testJson[MasterCommand](ReplaceRepo(
      VersionId("1"),
      objects = SignedString(
        string = """{"TYPE": "Workflow", ...}""",
        GenericSignature(
          "PGP",
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
            "string": "{\"TYPE\": \"Workflow\", ...}",
            "signature": {
              "TYPE": "PGP",
              "signatureString": "-----BEGIN PGP SIGNATURE-----\n\n...\n-----END PGP SIGNATURE-----\n"
            }
          }
        ]
      }""")
  }

  "UpdateRepo" in {
    testJson[MasterCommand](UpdateRepo(
      VersionId("1"),
      change = SignedString(
        string = """{"TYPE": "Workflow", ...}""",
        GenericSignature(
          "PGP",
           """-----BEGIN PGP SIGNATURE-----
            |
            |...
            |-----END PGP SIGNATURE-----
            |""".stripMargin)) :: Nil,
      delete = WorkflowPath("/WORKFLOW-A") :: AgentRefPath("/AGENT-A") :: Nil),
      json"""{
        "TYPE": "UpdateRepo",
        "versionId": "1",
        "change": [
          {
            "string": "{\"TYPE\": \"Workflow\", ...}",
            "signature": {
              "TYPE": "PGP",
              "signatureString": "-----BEGIN PGP SIGNATURE-----\n\n...\n-----END PGP SIGNATURE-----\n"
            }
          }
        ],
        "delete": [
          {
            "TYPE": "WorkflowPath",
            "path": "/WORKFLOW-A"
          }, {
            "TYPE": "AgentRefPath",
            "path": "/AGENT-A"
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

  "SwitchoverToBackup" in {
    testJson[MasterCommand](SwitchoverToBackup,
      json"""{
        "TYPE": "SwitchoverToBackup"
      }""")
  }

  "TakeSnapshot" in {
    testJson[MasterCommand](TakeSnapshot,
      json"""{
        "TYPE": "TakeSnapshot"
      }""")
  }

  "Shutdown" in {
    testJson[MasterCommand](Shutdown,
      json"""{
        "TYPE": "Shutdown"
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
