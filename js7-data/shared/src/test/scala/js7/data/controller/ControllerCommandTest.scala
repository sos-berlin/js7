package js7.data.controller

import js7.base.circeutils.CirceUtils.*
import js7.base.log.{CorrelId, CorrelIdWrapped}
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.web.Uri
import js7.data.agent.AgentPath
import js7.data.board.{BoardPath, NoticeId}
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.controller.ControllerCommand.*
import js7.data.item.VersionId
import js7.data.node.NodeId
import js7.data.order.OrderEvent.OrderResumed
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.value.NamedValues
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.{Label, Position}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class ControllerCommandTest extends OurTestSuite
{
  "Batch" - {
    "Batch" in {
      testJson[ControllerCommand](Batch(
        Seq(
          CorrelIdWrapped(CorrelId.empty, NoOperation()),
          CorrelIdWrapped(CorrelId("_CORREL_"), EmergencyStop()))),
        json"""{
          "TYPE": "Batch",
          "commands": [
            { "TYPE": "NoOperation" },
            { "TYPE": "EmergencyStop", "correlId": "_CORREL_" }
          ]
        }""")
    }

    "Batch.toString" in {
      assert(Batch(
        List(
          CorrelIdWrapped(CorrelId("_CORREL_"), NoOperation()),
          CorrelIdWrapped(CorrelId.empty, EmergencyStop()),
          CorrelIdWrapped(CorrelId.empty, NoOperation())))
        .toString == "Batch(NoOperation, EmergencyStop, NoOperation)")

      assert(Batch(
        List(
          CorrelIdWrapped(CorrelId.empty, NoOperation()),
          CorrelIdWrapped(CorrelId.empty, EmergencyStop()),
          CorrelIdWrapped(CorrelId.empty, NoOperation()),
          CorrelIdWrapped(CorrelId.empty, NoOperation())))
        .toString == "Batch(NoOperation, EmergencyStop, 2×NoOperation)")
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
      testJson[ControllerCommand](AddOrder(FreshOrder(OrderId("ORDER"), WorkflowPath("WORKFLOW"))),
        json"""{
          "TYPE": "AddOrder",
          "order": {
            "id": "ORDER",
            "workflowPath": "WORKFLOW"
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
      testJson[ControllerCommand](CancelOrders(Seq(OrderId("A"), OrderId("B")), CancellationMode.FreshOrStarted()),
        json"""{
          "TYPE": "CancelOrders",
          "orderIds": [ "A", "B" ]
        }""")
    }

    "CancelOrders FreshOrStarted" in {
      testJson[ControllerCommand](CancelOrders(
        Seq(OrderId("ORDER")),
        CancellationMode.FreshOrStarted(
          Some(CancellationMode.Kill(
            immediately = true,
            Some(WorkflowPath("WORKFLOW") ~ VersionId("VERSION") /: Position(1)))))),
        json"""{
          "TYPE": "CancelOrders",
          "orderIds": [ "ORDER" ],
          "mode": {
            "TYPE": "FreshOrStarted",
            "kill": {
              "immediately": true,
              "workflowPosition": {
                "workflowId": {
                  "path": "WORKFLOW",
                  "versionId": "VERSION"
                },
                "position": [ 1 ]
              }
            }
          }
        }""")

      testJsonDecoder[ControllerCommand](CancelOrders(
        Seq(OrderId("A"), OrderId("B")),
        CancellationMode.FreshOrStarted(
          Some(CancellationMode.Kill()))),
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

  "PostNotice" in {
    testJson[ControllerCommand](PostNotice(
        BoardPath("BOARD"),
        NoticeId("NOTICE")),
      json"""{
        "TYPE": "PostNotice",
        "boardPath": "BOARD",
        "noticeId": "NOTICE"
      }""")

    testJson[ControllerCommand](PostNotice(
        BoardPath("BOARD"),
        NoticeId("NOTICE"), Some(Timestamp("1970-01-01T01:00:00Z"))),
      json"""{
        "TYPE": "PostNotice",
        "boardPath": "BOARD",
        "noticeId": "NOTICE",
        "endOfLife": 3600000
      }""")
  }

  "DeleteNotice" in {
    testJson[ControllerCommand](DeleteNotice(BoardPath("BOARD"), NoticeId("NOTICE")),
      json"""{
        "TYPE": "DeleteNotice",
        "boardPath": "BOARD",
        "noticeId": "NOTICE"
      }""")
  }

  "DeleteOrdersWhenTerminated" in {
    testJson[ControllerCommand](DeleteOrdersWhenTerminated(Seq(OrderId("A"), OrderId("B"))),
      json"""{
        "TYPE": "DeleteOrdersWhenTerminated",
        "orderIds": [ "A", "B" ]
      }""")
  }

  "AnswerOrderPrompt" in {
    testJson[ControllerCommand](AnswerOrderPrompt(OrderId("ORDER")),
      json"""{
        "TYPE": "AnswerOrderPrompt",
        "orderId": "ORDER"
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

  "NoOperation()" in {
    testJson[ControllerCommand](NoOperation(Some(3.s)),
      json"""{
        "TYPE": "NoOperation",
        "duration": 3
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

  "ResetAgent" in {
    testJson[ControllerCommand](ResetAgent(AgentPath("AGENT"), force = true),
      json"""{
        "TYPE": "ResetAgent",
        "agentPath": "AGENT",
        "force": true
      }""")

    testJsonDecoder[ControllerCommand](ResetAgent(AgentPath("AGENT")),
      json"""{
        "TYPE": "ResetAgent",
        "agentPath": "AGENT"
      }""")
  }

  "ResumeOrder" in {
    testJson[ControllerCommand](
      ResumeOrder(
        OrderId("ORDER"),
        Some(Position(1)),
        Seq(
          OrderResumed.ReplaceHistoricOutcome(Position(0), Outcome.Succeeded(NamedValues.rc(0))),
          OrderResumed.ReplaceHistoricOutcome(Position(1), Outcome.Failed(NamedValues.rc(1)))),
        asSucceeded = true),
      json"""{
        "TYPE": "ResumeOrder",
        "orderId": "ORDER",
        "position": [ 1 ],
        "historyOperations": [
          {
            "TYPE": "Replace",
            "position": [ 0 ],
            "outcome": {
              "TYPE": "Succeeded",
              "namedValues": {
                "returnCode": 0
              }
            }
          }, {
            "TYPE": "Replace",
            "position": [ 1 ],
            "outcome": {
              "TYPE": "Failed",
              "namedValues": {
                "returnCode": 1
              }
            }
          }
        ],
        "asSucceeded": true
      }""")

    testJsonDecoder[ControllerCommand](
      ResumeOrder(OrderId("ORDER")),
      json"""{
        "TYPE": "ResumeOrder",
        "orderId": "ORDER"
      }""")
  }

  "ResumeOrders" in {
    testJson[ControllerCommand](ResumeOrders(
      Seq(OrderId("A"), OrderId("B")),
      asSucceeded = true),
      json"""{
        "TYPE": "ResumeOrders",
        "orderIds": [ "A", "B" ],
        "asSucceeded": true
      }""")

    testJsonDecoder[ControllerCommand](ResumeOrders(Seq(OrderId("A"), OrderId("B"))), json"""
      {
        "TYPE": "ResumeOrders",
        "orderIds": [ "A", "B" ]
      }""")
  }

  "SuspendOrders" in {
    testJson[ControllerCommand](SuspendOrders(Seq(OrderId("A"), OrderId("B"))), json"""
      {
        "TYPE": "SuspendOrders",
        "orderIds": [ "A", "B" ],
        "mode": {}
      }""")

    testJson[ControllerCommand](SuspendOrders(Seq(OrderId("A")), SuspensionMode(Some(CancellationMode.Kill()))), json"""
      {
        "TYPE": "SuspendOrders",
        "orderIds": [ "A" ],
        "mode": {
          "kill": {
            "immediately": false
          }
        }
      }""")
  }

  "ControlWorkflowPath" in {
    testJson[ControllerCommand](
      ControlWorkflowPath(
        WorkflowPath("WORKFLOW"),
        suspend = Some(true),
        skip = Map(
          Label("LABEL") -> true)),
      json"""{
        "TYPE": "ControlWorkflowPath",
        "workflowPath": "WORKFLOW",
        "suspend": true,
        "skip": {
          "LABEL": true
        }
      }""")
  }

  "ControlWorkflow" in {
    testJson[ControllerCommand](
      ControlWorkflow(
        WorkflowPath("WORKFLOW") ~ "VERSION",
        addBreakpoints = Set(Position(1)),
        removeBreakpoints = Set(Position(2))),
      json"""{
        "TYPE": "ControlWorkflow",
        "workflowId": {
          "path": "WORKFLOW",
          "versionId": "VERSION"
        },
        "addBreakpoints": [ [ 1 ] ],
        "removeBreakpoints": [ [ 2 ] ]
      }""")
  }

  "ClusterAppointNodes" in {
    testJson[ControllerCommand](
      ClusterAppointNodes(
        Map(
          NodeId("A") -> Uri("https://A"),
          NodeId("B") -> Uri("https://B")),
        activeId = NodeId("A")),
      json"""{
        "TYPE": "ClusterAppointNodes",
        "idToUri": {
          "A": "https://A",
          "B": "https://B"
        },
        "activeId": "A"
      }""")
  }

  "ClusterSwitchOver" in {
    testJson[ControllerCommand](ClusterSwitchOver,
      json"""{
        "TYPE": "ClusterSwitchOver"
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
