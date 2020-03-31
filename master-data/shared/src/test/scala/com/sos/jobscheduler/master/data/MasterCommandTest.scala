package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.cluster.{ClusterNodeId, ClusterState}
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.crypt.{GenericSignature, SignedString}
import com.sos.jobscheduler.data.event.JournalPosition
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.data.MasterCommand._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

final class MasterCommandTest extends FreeSpec
{
  "ClusterAppointNodes" in {
    testJson[MasterCommand](ClusterAppointNodes(
      Map(
        ClusterNodeId("A") ->Uri("http://ACTIVE"),
        ClusterNodeId("B") -> Uri("http://B")),
      ClusterNodeId("A")),
      json"""{
        "TYPE": "ClusterAppointNodes",
        "idToUri": {
          "A": "http://ACTIVE",
          "B": "http://B"
        },
        "activeId": "A"
      }""")
  }

  "ClusterSwitchOver" in {
    testJson[MasterCommand](ClusterSwitchOver,
      json"""{
        "TYPE": "ClusterSwitchOver"
      }""")
  }

  "Batch" - {
    "Batch" in {
      testJson[MasterCommand](Batch(List(NoOperation, EmergencyStop())),
        json"""{
          "TYPE": "Batch",
          "commands": [
            { "TYPE": "NoOperation" },
            { "TYPE": "EmergencyStop" }
          ]
        }""")
    }

    "Batch.toString" in {
      assert(Batch(List(NoOperation, EmergencyStop(), NoOperation)).toString == "Batch(NoOperation, EmergencyStop, NoOperation)")
      assert(Batch(List(NoOperation, EmergencyStop(), NoOperation, NoOperation)).toString == "Batch(NoOperation, EmergencyStop, 2×NoOperation)")
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

  "ClusterStartBackupNode" in {
    testJson[MasterCommand](
      ClusterStartBackupNode(
        Map(
          ClusterNodeId("A") -> Uri("http://A"),
          ClusterNodeId("B") -> Uri("http://B")),
        ClusterNodeId("A")),
      json"""{
        "TYPE": "ClusterStartBackupNode",
        "idToUri": {
          "A": "http://A",
          "B": "http://B"
        },
        "activeId": "A"
      }""")
  }

  "ClusterPrepareCoupling" in {
    testJson[MasterCommand](
      ClusterPrepareCoupling(ClusterNodeId("A"), ClusterNodeId("B")),
      json"""{
        "TYPE": "ClusterPrepareCoupling",
        "activeId": "A",
        "passiveId": "B"
      }""")
  }

  "ClusterCouple" in {
    testJson[MasterCommand](
      ClusterCouple(ClusterNodeId("A"), ClusterNodeId("B")),
      json"""{
        "TYPE": "ClusterCouple",
        "activeId": "A",
        "passiveId": "B"
      }""")
  }

  "ClusterRecouple" in {
    testJson[MasterCommand](
      ClusterRecouple(ClusterNodeId("A"), ClusterNodeId("B")),
      json"""{
        "TYPE": "ClusterRecouple",
        "activeId": "A",
        "passiveId": "B"
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

  "ClusterInhibitActivation" in {
    testJson[MasterCommand](ClusterInhibitActivation(7.s),
      json"""{
        "TYPE": "ClusterInhibitActivation",
        "duration": 7
      }""")
  }

  "ClusterInhibitActivation.Response" in {
    testJson[MasterCommand.Response](ClusterInhibitActivation.Response(Some(ClusterState.FailedOver(
      Map(
        ClusterNodeId("A") -> Uri("http://A"),
        ClusterNodeId("B") -> Uri("http://B")),
      activeId = ClusterNodeId("A"),
      JournalPosition(0, 1000)))),
      json"""{
        "TYPE": "ClusterInhibitActivationResponse",
        "failedOver": {
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A",
          "failedAt": {
            "fileEventId": 0,
            "position": 1000
          }
        }
      }""")
  }

  "EmergencyStop" - {
    "restart=false" in {
      testJson[MasterCommand](EmergencyStop(),
        json"""{
          "TYPE": "EmergencyStop"
        }""")
    }

    "restart=true" in {
      testJson[MasterCommand](EmergencyStop(restart = true),
        json"""{
          "TYPE": "EmergencyStop",
          "restart": true
        }""")
    }
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

  "TakeSnapshot" in {
    testJson[MasterCommand](TakeSnapshot,
      json"""{
        "TYPE": "TakeSnapshot"
      }""")
  }

  "ShutDown" - {
    "restart=false" in {
      testJson[MasterCommand](ShutDown(),
        json"""{
          "TYPE": "ShutDown"
        }""")
    }

    "restart=true" in {
      testJson[MasterCommand](ShutDown(restart = true),
        json"""{
          "TYPE": "ShutDown",
          "restart": true
        }""")
    }
  }

  "Response.Accepted" in {
    testJson[MasterCommand.Response](
      Response.Accepted,
      json"""{
        "TYPE": "Accepted"
      }""")
  }
}
