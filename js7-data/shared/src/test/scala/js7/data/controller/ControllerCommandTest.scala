package js7.data.controller

import js7.base.circeutils.CirceUtils.*
import js7.base.log.{CorrelId, CorrelIdWrapped}
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.base.web.Uri
import js7.data.agent.AgentPath
import js7.data.board.{BoardPath, GlobalBoard, GlobalNoticeKey, NoticeKey, PlannableBoard}
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.controller.ControllerCommand.*
import js7.data.item.VersionId
import js7.data.node.NodeId
import js7.data.order.OrderEvent.OrderResumed
import js7.data.order.{FreshOrder, OrderId, OrderOutcome}
import js7.data.plan.PlanSchemaId
import js7.data.value.NamedValues
import js7.data.value.expression.ExprFunction.testing.|=>
import js7.data.value.expression.Expression.exprFun
import js7.data.value.expression.ExpressionParser
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.{Label, Position}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class ControllerCommandTest extends OurTestSuite:

  "Batch" - {
    "Batch" in:
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

    "Batch.toString" in:
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

    "BatchResponse" in:
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

    "BatchResponse.toString" in:
      val threeResponses = Right(Response.Accepted) :: Left(Problem("PROBLEM")) :: Right(Response.Accepted) :: Nil
      assert(Batch.Response(threeResponses).toString == "BatchResponse(2 succeeded and 1 failed)")
      assert(Batch.Response(threeResponses ::: Right(Response.Accepted) :: Nil).toString == "BatchResponse(3 succeeded and 1 failed)")
  }

  "AddOrder" - {
    "AddOrder" in:
      testJson[ControllerCommand](AddOrder(FreshOrder(OrderId("ORDER"), WorkflowPath("WORKFLOW"))),
        json"""{
          "TYPE": "AddOrder",
          "order": {
            "id": "ORDER",
            "workflowPath": "WORKFLOW"
          }
        }""")

    "AddOrder.Response" in:
      testJson[ControllerCommand.Response](AddOrder.Response(true),
        json"""{
          "TYPE": "AddOrder.Response",
          "ignoredBecauseDuplicate": true
        }""")
  }

  "CancelOrders" - {
    "CancelOrders FreshOnly" in:
      testJson[ControllerCommand](CancelOrders(Seq(OrderId("A"), OrderId("B")), CancellationMode.FreshOrStarted()),
        json"""{
          "TYPE": "CancelOrders",
          "orderIds": [ "A", "B" ]
        }""")

    "CancelOrders FreshOrStarted" in:
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

  "PostNotice" in:
    testJson[ControllerCommand](
      PostNotice(
        BoardPath("BOARD") / GlobalNoticeKey("NOTICE"),
        endOfLife = None),
      json"""{
        "TYPE": "PostNotice",
        "noticeId": [ "BOARD", "NOTICE" ]
      }""")

    testJson[ControllerCommand](
      PostNotice(
        BoardPath("BOARD") / GlobalNoticeKey("NOTICE"),
        Some(ts"1970-01-01T01:00:00Z")),
      json"""{
        "TYPE": "PostNotice",
        "noticeId": [ "BOARD", "NOTICE" ],
        "endOfLife": 3600000
      }""")

    testJsonDecoder[ControllerCommand](
      PostNotice(
        BoardPath("BOARD") / GlobalNoticeKey("NOTICE"),
        endOfLife = None),
      json"""{
        "TYPE": "PostNotice",
        "boardPath": "BOARD",
        "noticeId": "NOTICE"
      }""")

    // COMPATIBLE with v2.7.3
    testJsonDecoder[ControllerCommand](
      PostNotice(
        BoardPath("BOARD") / GlobalNoticeKey("NOTICE"),
        Some(ts"1970-01-01T01:00:00Z")),
      json"""{
        "TYPE": "PostNotice",
        "boardPath": "BOARD",
        "noticeId": "NOTICE",
        "endOfLife": 3600000
      }""")

  "DeleteNotice" in:
    testJson[ControllerCommand](
      DeleteNotice(PlanSchemaId("DailyPlan") / "2025-01-17" / BoardPath("BOARD") / NoticeKey("NOTICE")),
      json"""{
        "TYPE": "DeleteNotice",
        "noticeId": [ "DailyPlan", "2025-01-17", "BOARD", "NOTICE" ]
      }""")

    // COMPATIBLE with v2.7.3
    testJsonDecoder[ControllerCommand](
      DeleteNotice:
        BoardPath("BOARD") / GlobalNoticeKey("NOTICE"),
      json"""{
        "TYPE": "DeleteNotice",
        "boardPath": "BOARD",
        "noticeId": "NOTICE"
      }""")

  "ChangeGlobalToPlannableBoard" in:
    testJson[ControllerCommand](
      ChangeGlobalToPlannableBoard(
        PlannableBoard(BoardPath("BOARD")),
        PlanSchemaId("DailyPlan"),
        "noticeKey" |=> expr("[ substring($noticeKey, 0, 10), substring($noticeKey, 10) ]")
      ),
      json"""{
        "TYPE": "ChangeGlobalToPlannableBoard",
        "plannableBoard": {
          "path": "BOARD",
          "expectOrderToNoticeKey": "\"\"",
          "postOrderToNoticeKey": "\"\""
        },
        "planSchemaId": "DailyPlan",
        "splitNoticeKey": "noticeKey => [substring($$noticeKey, 0, 10), substring($$noticeKey, 10)]"
      }""")

  "ChangePlannableToGlobalBoard" in:
    testJson[ControllerCommand](
      ChangePlannableToGlobalBoard(
        GlobalBoard(
          BoardPath("BOARD"),
          postOrderToNoticeKey = expr("'NOTICEKEY'"),
          expectOrderToNoticeKey = expr("'NOTICEKEY'"),
          endOfLife = expr("$js7EpochMilli + 24 * 3600 * 1000")),
        PlanSchemaId("DailyPlan"),
        exprFun""" (planKey, noticeKey) => "$$planKey$$noticeKey" """),
      json"""{
        "TYPE": "ChangePlannableToGlobalBoard",
        "globalBoard": {
          "path": "BOARD",
          "expectOrderToNoticeKey": "'NOTICEKEY'",
          "postOrderToNoticeKey": "'NOTICEKEY'",
          "endOfLife": "$$js7EpochMilli + 24 * 3600 * 1000"
        },
        "planSchemaId": "DailyPlan",
        "makeNoticeKey": "(planKey,noticeKey) => \"$$planKey$$noticeKey\""
      }""")

  "DeleteOrdersWhenTerminated" in:
    testJson[ControllerCommand](DeleteOrdersWhenTerminated(Seq(OrderId("A"), OrderId("B"))),
      json"""{
        "TYPE": "DeleteOrdersWhenTerminated",
        "orderIds": [ "A", "B" ]
      }""")

  "AnswerOrderPrompt" in:
    testJson[ControllerCommand](AnswerOrderPrompt(OrderId("ORDER")),
      json"""{
        "TYPE": "AnswerOrderPrompt",
        "orderId": "ORDER"
      }""")

  "EmergencyStop" - {
    "restart=false" in:
      testJson[ControllerCommand](EmergencyStop(),
        json"""{
          "TYPE": "EmergencyStop"
        }""")

    "restart=true" in:
      testJson[ControllerCommand](EmergencyStop(restart = true),
        json"""{
          "TYPE": "EmergencyStop",
          "restart": true
        }""")
  }

  "ReleaseEvents" in:
    testJson[ControllerCommand](
      ReleaseEvents(123L),
      json"""{
        "TYPE": "ReleaseEvents",
        "untilEventId": 123
      }""")

  "NoOperation()" in:
    testJson[ControllerCommand](NoOperation(Some(3.s)),
      json"""{
        "TYPE": "NoOperation",
        "duration": 3
      }""")

  "EmitTestEvent" in:  // For tests only
    testJson[ControllerCommand](EmitTestEvent,
      json"""{
        "TYPE": "EmitTestEvent"
      }""")

  "TakeSnapshot" in:
    testJson[ControllerCommand](TakeSnapshot,
      json"""{
        "TYPE": "TakeSnapshot"
      }""")

  "ShutDown" - {
    "restart=false" in:
      testJson[ControllerCommand](ShutDown(),
        json"""{
          "TYPE": "ShutDown"
        }""")

    "restart=true" in:
      testJson[ControllerCommand](ShutDown(restart = true),
        json"""{
          "TYPE": "ShutDown",
          "restart": true
        }""")

    "clusterAction=Switchover" in:
      testJson[ControllerCommand](ShutDown(clusterAction = Some(ShutDown.ClusterAction.Switchover)),
        json"""{
          "TYPE": "ShutDown",
          "clusterAction": {
            "TYPE": "Switchover"
          }
        }""")

    "clusterAction=Failover" in:
      testJson[ControllerCommand](ShutDown(clusterAction = Some(ShutDown.ClusterAction.Failover)),
        json"""{
          "TYPE": "ShutDown",
          "clusterAction": {
            "TYPE": "Failover"
          }
        }""")

    "suppressSnapshot=true, dontNotifyActiveNode=true" in:
      testJson[ControllerCommand](ShutDown(suppressSnapshot = true, dontNotifyActiveNode = true),
        json"""{
          "TYPE": "ShutDown",
          "suppressSnapshot": true,
          "dontNotifyActiveNode": true
        }""")

    "suppressSnapshot=true" in:
      testJson[ControllerCommand](ShutDown(suppressSnapshot = true),
        json"""{
          "TYPE": "ShutDown",
          "suppressSnapshot": true
        }""")
  }

  "ResetAgent" in:
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

  "GoOrder" in:
    testJsonDecoder[ControllerCommand](
      GoOrder(OrderId("ORDER"), Position(1)),
      json"""{
        "TYPE": "GoOrder",
        "orderId": "ORDER",
        "position": [ 1 ]
      }""")

  "ResumeOrder" in:
    testJson[ControllerCommand](
      ResumeOrder(
        OrderId("ORDER"),
        Some(Position(1)),
        Seq(
          OrderResumed.ReplaceHistoricOutcome(Position(0), OrderOutcome.Succeeded(NamedValues.rc(0))),
          OrderResumed.ReplaceHistoricOutcome(Position(1), OrderOutcome.Failed(NamedValues.rc(1)))),
        asSucceeded = true,
        restartKilledJob = Some(false)),
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
        "asSucceeded": true,
        "restartKilledJob": false
      }""")

    testJsonDecoder[ControllerCommand](
      ResumeOrder(OrderId("ORDER")),
      json"""{
        "TYPE": "ResumeOrder",
        "orderId": "ORDER"
      }""")

  "ResumeOrders" in:
    testJson[ControllerCommand](ResumeOrders(
      Seq(OrderId("A"), OrderId("B")),
      asSucceeded = true,
      restartKilledJob = Some(true)),
      json"""{
        "TYPE": "ResumeOrders",
        "orderIds": [ "A", "B" ],
        "asSucceeded": true,
        "restartKilledJob": true
      }""")

    testJsonDecoder[ControllerCommand](ResumeOrders(Seq(OrderId("A"), OrderId("B"))), json"""
      {
        "TYPE": "ResumeOrders",
        "orderIds": [ "A", "B" ]
      }""")

  "SuspendOrders" in:
    testJson[ControllerCommand](
      SuspendOrders(
        Seq(OrderId("A")),
        SuspensionMode(resetState = true, Some(CancellationMode.Kill()))),
      json"""
      {
        "TYPE": "SuspendOrders",
        "orderIds": [ "A" ],
        "mode": {
          "resetState": true,
          "kill": {
            "immediately": false
          }
        }
      }""")

    testJsonDecoder[ControllerCommand](SuspendOrders(Seq(OrderId("A"), OrderId("B"))),
      json"""
      {
        "TYPE": "SuspendOrders",
        "orderIds": [ "A", "B" ],
        "mode": {}
      }""")

  "TransferOrders" in:
    testJson[ControllerCommand](
      TransferOrders(WorkflowPath("WORKFLOW") ~ "v1"), json"""
      {
        "TYPE": "TransferOrders",
        "workflowId": {
          "path": "WORKFLOW",
          "versionId": "v1"
        }
      }""")

  "ControlWorkflowPath" in:
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

  "ControlWorkflow" in:
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

  "ClusterAppointNodes" in:
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

  "ClusterSwitchOver" in:
    testJson[ControllerCommand](ClusterSwitchOver(Some(AgentPath("AGENT"))),
      json"""{
        "TYPE": "ClusterSwitchOver",
        "agentPath": "AGENT"
      }""")

    testJson[ControllerCommand](ClusterSwitchOver(),
      json"""{
        "TYPE": "ClusterSwitchOver"
      }""")

  "ConfirmClusterNodeLoss" in:
    testJson[ControllerCommand](ConfirmClusterNodeLoss(AgentPath("AGENT"), NodeId.primary, "me"),
      json"""{
        "TYPE": "ConfirmClusterNodeLoss",
        "agentPath": "AGENT",
        "lostNodeId": "Primary",
        "confirmer": "me"
      }""")

  "Response.Accepted" in:
    testJson[ControllerCommand.Response](
      Response.Accepted,
      json"""{
        "TYPE": "Accepted"
      }""")
