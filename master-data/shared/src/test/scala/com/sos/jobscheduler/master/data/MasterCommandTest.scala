package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.crypt.{GenericSignature, SignedString}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.cluster.{ClusterCommand, ClusterNodeId}
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.data.MasterCommand._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class MasterCommandTest extends AnyFreeSpec
{
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

  "AddOrder" - {
    "AddOrder" in {
      testJson[MasterCommand](AddOrder(FreshOrder(OrderId("ORDER"), WorkflowPath("/WORKFLOW"))),
        json"""{
          "TYPE": "AddOrder",
          "order": {
            "id": "ORDER",
            "workflowPath": "/WORKFLOW"
          }
        }""")
    }

    "AddOrder.Response" in {
      testJson[MasterCommand.Response](AddOrder.Response(true),
        json"""{
          "TYPE": "AddOrder.Response",
          "ignoredBecauseDuplicate": true
        }""")
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

  "ReleaseEvents" in {
    testJson[MasterCommand](
      ReleaseEvents(123L),
      json"""{
        "TYPE": "ReleaseEvents",
        "untilEventId": 123
      }""")
  }

  "NoOperation" in {
    testJson[MasterCommand](NoOperation,
      json"""{
        "TYPE": "NoOperation"
      }""")
  }

  "EmitTestEvent" in {  // For tests only
    testJson[MasterCommand](EmitTestEvent,
      json"""{
        "TYPE": "EmitTestEvent"
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

    "clusterAction=Switchover" in {
      testJson[MasterCommand](ShutDown(clusterAction = Some(ShutDown.ClusterAction.Switchover)),
        json"""{
          "TYPE": "ShutDown",
          "clusterAction": {
            "TYPE": "Switchover"
          }
        }""")
    }

    "clusterAction=Failover" in {
      testJson[MasterCommand](ShutDown(clusterAction = Some(ShutDown.ClusterAction.Failover)),
        json"""{
          "TYPE": "ShutDown",
          "clusterAction": {
            "TYPE": "Failover"
          }
        }""")
    }
  }

  "ClusterAppointNodes" in {
    testJson[MasterCommand](
      ClusterAppointNodes(
        Map(
          ClusterNodeId("A") -> Uri("http://A"),
          ClusterNodeId("B") -> Uri("http://B")),
        ClusterNodeId("A")),
      json"""{
        "TYPE": "ClusterAppointNodes",
        "idToUri": {
          "A": "http://A",
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

  // Internal use only
  "InternalClusterCommand" in {
    testJson[MasterCommand](InternalClusterCommand(
      ClusterCommand.ClusterCouple(
        ClusterNodeId("A"),
        ClusterNodeId("B"))),
      json"""{
        "TYPE": "InternalClusterCommand",
        "clusterCommand": {
          "TYPE": "ClusterCouple",
          "activeId": "A",
          "passiveId": "B"
        }
      }""")
  }

  "InternalClusterCommand.Response" in {
    testJson[MasterCommand.Response](InternalClusterCommand.Response(
      ClusterCommand.Response.Accepted),
      json"""{
        "TYPE": "InternalClusterCommand.Response",
        "response": {
          "TYPE": "Accepted"
        }
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
