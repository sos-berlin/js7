package js7.controller.data

import js7.base.circeutils.CirceUtils._
import js7.base.crypt.{GenericSignature, SignedString}
import js7.base.problem.Problem
import js7.base.web.Uri
import js7.controller.data.ControllerCommand._
import js7.data.agent.AgentRefPath
import js7.data.cluster.{ClusterCommand, ClusterSetting}
import js7.data.command.CancelMode
import js7.data.item.VersionId
import js7.data.node.NodeId
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.{BranchId, Position}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}
import org.scalatest.freespec.AnyFreeSpec

final class ControllerCommandTest extends AnyFreeSpec
{
  "Batch" - {
    "Batch" in {
      testJson[ControllerCommand](Batch(List(NoOperation, EmergencyStop())),
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
      testJson[ControllerCommand.Response](Batch.Response(Right(Response.Accepted) :: Left(Problem("PROBLEM")) :: Nil),
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
      testJson[ControllerCommand](AddOrder(FreshOrder(OrderId("ORDER"), WorkflowPath("/WORKFLOW"))),
        json"""{
          "TYPE": "AddOrder",
          "order": {
            "id": "ORDER",
            "workflowPath": "/WORKFLOW"
          }
        }""")
    }

    "AddOrder.Response" in {
      testJson[ControllerCommand.Response](AddOrder.Response(true),
        json"""{
          "TYPE": "AddOrder.Response",
          "ignoredBecauseDuplicate": true
        }""")
    }
  }

  "CancelOrders" - {
    "CancelOrders FreshOnly" in {
      testJson[ControllerCommand](CancelOrders(Seq(OrderId("A"), OrderId("B")), CancelMode.FreshOrStarted()),
        json"""{
          "TYPE": "CancelOrders",
          "orderIds": [ "A", "B" ]
        }""")
    }

    "CancelOrders FreshOrStarted" in {
      testJson[ControllerCommand](CancelOrders(
        Seq(OrderId("ORDER")),
        CancelMode.FreshOrStarted(
          Some(CancelMode.Kill(
            immediately = true,
            Some(WorkflowPath("/WORKFLOW") ~ VersionId("VERSION") /: Position(1)))))),
        json"""{
          "TYPE": "CancelOrders",
          "orderIds": [ "ORDER" ],
          "mode": {
            "TYPE": "FreshOrStarted",
            "kill": {
              "immediately": true,
              "workflowPosition": {
                "workflowId": {
                  "path": "/WORKFLOW",
                  "versionId": "VERSION"
                },
                "position": [ 1 ]
              }
            }
          }
        }""")

      testJsonDecoder[ControllerCommand](CancelOrders(
        Seq(OrderId("A"), OrderId("B")),
        CancelMode.FreshOrStarted(
          Some(CancelMode.Kill()))),
        json"""{
          "TYPE": "CancelOrders",
          "orderIds": [ "A", "B" ],
          "mode": {
            "TYPE": "FreshOrStarted",
            "kill": {}
          }
        }""")
    }
  }

  "RemoveOrdersWhenTerminated" in {
    testJson[ControllerCommand](RemoveOrdersWhenTerminated(Seq(OrderId("A"), OrderId("B"))),
      json"""{
        "TYPE": "RemoveOrdersWhenTerminated",
        "orderIds": [ "A", "B" ]
      }""")
  }

  "ReplaceRepo" in {
    testJson[ControllerCommand](ReplaceRepo(
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
    testJson[ControllerCommand](UpdateRepo(
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
      testJson[ControllerCommand](EmergencyStop(),
        json"""{
          "TYPE": "EmergencyStop"
        }""")
    }

    "restart=true" in {
      testJson[ControllerCommand](EmergencyStop(restart = true),
        json"""{
          "TYPE": "EmergencyStop",
          "restart": true
        }""")
    }
  }

  "ReleaseEvents" in {
    testJson[ControllerCommand](
      ReleaseEvents(123L),
      json"""{
        "TYPE": "ReleaseEvents",
        "untilEventId": 123
      }""")
  }

  "NoOperation" in {
    testJson[ControllerCommand](NoOperation,
      json"""{
        "TYPE": "NoOperation"
      }""")
  }

  "EmitTestEvent" in {  // For tests only
    testJson[ControllerCommand](EmitTestEvent,
      json"""{
        "TYPE": "EmitTestEvent"
      }""")
  }

  "TakeSnapshot" in {
    testJson[ControllerCommand](TakeSnapshot,
      json"""{
        "TYPE": "TakeSnapshot"
      }""")
  }

  "ShutDown" - {
    "restart=false" in {
      testJson[ControllerCommand](ShutDown(),
        json"""{
          "TYPE": "ShutDown"
        }""")
    }

    "restart=true" in {
      testJson[ControllerCommand](ShutDown(restart = true),
        json"""{
          "TYPE": "ShutDown",
          "restart": true
        }""")
    }

    "clusterAction=Switchover" in {
      testJson[ControllerCommand](ShutDown(clusterAction = Some(ShutDown.ClusterAction.Switchover)),
        json"""{
          "TYPE": "ShutDown",
          "clusterAction": {
            "TYPE": "Switchover"
          }
        }""")
    }

    "clusterAction=Failover" in {
      testJson[ControllerCommand](ShutDown(clusterAction = Some(ShutDown.ClusterAction.Failover)),
        json"""{
          "TYPE": "ShutDown",
          "clusterAction": {
            "TYPE": "Failover"
          }
        }""")
    }

    "suppressSnapshot=true" in {
      testJson[ControllerCommand](ShutDown(suppressSnapshot = true),
        json"""{
          "TYPE": "ShutDown",
          "suppressSnapshot": true
        }""")
    }
  }

  "ResumeOrders" in {
    testJson[ControllerCommand](ResumeOrders(Seq(OrderId("A"), OrderId("B")), Some(Position(1) / BranchId.Try_ % 2)), json"""
      {
        "TYPE": "ResumeOrders",
        "orderIds": [ "A", "B" ],
        "position": [ 1, "try", 2]
      }""")
  }

  "SuspendOrders" in {
    testJson[ControllerCommand](SuspendOrders(Seq(OrderId("A"), OrderId("B"))), json"""
      {
        "TYPE": "SuspendOrders",
        "orderIds": [ "A", "B" ]
      }""")
  }

  "ClusterAppointNodes" in {
    testJson[ControllerCommand](
      ClusterAppointNodes(
        ClusterSetting(
          Map(
            NodeId("A") -> Uri("http://A"),
            NodeId("B") -> Uri("http://B")),
          activeId = NodeId("A"))),
      json"""{
        "TYPE": "ClusterAppointNodes",
        "setting": {
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A"
        }
      }""")
  }

  "ClusterSwitchOver" in {
    testJson[ControllerCommand](ClusterSwitchOver,
      json"""{
        "TYPE": "ClusterSwitchOver"
      }""")
  }

  // Internal use only
  "InternalClusterCommand" in {
    testJson[ControllerCommand](InternalClusterCommand(
      ClusterCommand.ClusterCouple(
        NodeId("A"),
        NodeId("B"))),
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
    testJson[ControllerCommand.Response](InternalClusterCommand.Response(
      ClusterCommand.Response.Accepted),
      json"""{
        "TYPE": "InternalClusterCommand.Response",
        "response": {
          "TYPE": "Accepted"
        }
      }""")
  }

  "Response.Accepted" in {
    testJson[ControllerCommand.Response](
      Response.Accepted,
      json"""{
        "TYPE": "Accepted"
      }""")
  }
}
