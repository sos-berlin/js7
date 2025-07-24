package js7.data.subagent

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.crypt.silly.SillySigner
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.base.utils.Base64UUID
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.{ControllerId, ControllerState}
import js7.data.event.JournalId
import js7.data.item.ItemSigner
import js7.data.order.{Order, OrderId}
import js7.data.other.HeartbeatTiming
import js7.data.subagent.SubagentCommand.{AttachSignedItem, CoupleDirector, DedicateSubagent, DetachProcessedOrder, DetachProcessedOrders, KillProcess, ShutDown, StartOrderProcess}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class SubagentCommandTest extends OurTestSuite:

  "JSON" - {
    "DedicateSubagent" in:
      testJson[SubagentCommand](
        DedicateSubagent(
          SubagentId("SUBAGENT"),
          AgentPath("AGENT"),
          AgentRunId(JournalId(Base64UUID.zero)),
          ControllerId("CONTROLLER")),
        json"""{
          "TYPE": "DedicateSubagent",
          "subagentId": "SUBAGENT",
          "agentPath": "AGENT",
          "agentRunId": "AAAAAAAAAAAAAAAAAAAAAA",
          "controllerId": "CONTROLLER"
        }""")

      testJsonDecoder[SubagentCommand](
        DedicateSubagent(
          SubagentId("SUBAGENT"),
          AgentPath("AGENT"),
          AgentRunId.pastAgentVersion,
          ControllerId("CONTROLLER")),
        json"""{
          "TYPE": "DedicateSubagent",
          "subagentId": "SUBAGENT",
          "agentPath": "AGENT",
          "controllerId": "CONTROLLER"
        }""")

    "CoupleDirector" in:
      testJson[SubagentCommand](
        CoupleDirector(
          SubagentId("SUBAGENT"),
          SubagentRunId(Base64UUID.zero),
          1001,
          HeartbeatTiming(1234.ms, 12345.ms)),
        json"""{
          "TYPE": "CoupleDirector",
          "subagentId": "SUBAGENT",
          "subagentRunId": "AAAAAAAAAAAAAAAAAAAAAA",
          "eventId": 1001,
          "heartbeatTiming": {
            "heartbeat": 1.234,
            "heartbeatTimeout": 12.345
          }
        }""")

    "AttachSignedItem" in:
      val itemSigner = new ItemSigner(SillySigner.Default, ControllerState.signableItemJsonCodec)
      testJson[SubagentCommand](
        AttachSignedItem(
          itemSigner.sign(Workflow(WorkflowPath("WORKFLOW") ~ "1", Nil))),
        json"""{
          "TYPE": "AttachSignedItem",
          "signed": {
            "signature": {
              "TYPE": "Silly",
              "signatureString": "SILLY-SIGNATURE"
            },
            "string": "{\"TYPE\":\"Workflow\",\"path\":\"WORKFLOW\",\"versionId\":\"1\",\"instructions\":[]}"
          }
        }""")

    //"AttachItem" in {
    //  testJson[SubagentCommand](
    //    AttachItem(Workflow(WorkflowPath("WORKFLOW") ~ "1", Nil)),
    //    json"""{
    //      "TYPE": "AttachItem",
    //      "item": {
    //        "TYPE": "Workflow",
    //        "path": "WORKFLOW",
    //        "versionId": "1",
    //        "instructions": []
    //      }
    //    }""")
    //}

    "ShutDown" in:
      testJson[SubagentCommand](
        ShutDown(Some(SIGTERM), dontWaitForDirector = true, restart = true),
        json"""{
          "TYPE": "ShutDown",
          "processSignal": "SIGTERM",
          "dontWaitForDirector": true,
          "restart": true
        } """)

      testJsonDecoder[SubagentCommand](
        ShutDown(),
        json"""{
          "TYPE": "ShutDown"
        } """)

    "StartOrderProcess" in:
      testJson[SubagentCommand](
        StartOrderProcess(
          Order(
            OrderId("ORDER"),
            (WorkflowPath("WORKFLOW") ~ "1") /: Position(0),
            Order.Processing(SubagentId("SUBAGENT"))),
          Map("expr" -> expr("'EXPR'")),
          Some(ts"2025-07-01T17:00:00Z")),
        json"""{
          "TYPE": "StartOrderProcess",
          "order": {
            "id": "ORDER",
            "state": {
              "TYPE": "Processing",
              "subagentId": "SUBAGENT"
            },
            "workflowPosition": {
              "workflowId": {
                "path": "WORKFLOW",
                "versionId": "1"
              },
              "position": [ 0 ]
            }
          },
          "defaultArguments": {
            "expr": "'EXPR'"
          },
          "endOfAdmissionPeriod": 1751389200000
        }""")

      // COMPATIBLE WITH v2.3.0: no correlId
      testJsonDecoder[SubagentCommand](
        StartOrderProcess(
          Order(
            OrderId("ORDER"),
            (WorkflowPath("WORKFLOW") ~ "1") /: Position(0),
            Order.Processing(SubagentId("SUBAGENT"))),
          Map("expr" -> expr("'EXPR'"))),
        json"""{
          "TYPE": "StartOrderProcess",
          "defaultArguments": {
            "expr": "'EXPR'"
          },
          "order": {
            "id": "ORDER",
            "state": {
              "TYPE": "Processing",
              "subagentId": "SUBAGENT"
            },
            "workflowPosition": {
              "workflowId": {
                "path": "WORKFLOW",
                "versionId": "1"
              },
              "position": [ 0 ]
            }
          }
        }""")

    "DetachProcessedOrder" in:
      testJson[SubagentCommand](
        DetachProcessedOrder(OrderId("ORDER")),
        json"""{
          "TYPE": "DetachProcessedOrder",
          "orderId": "ORDER"
        }""")

    "DetachProcessedOrders" in:
      testJson[SubagentCommand](
        DetachProcessedOrders(Seq(OrderId("ORDER"))),
        json"""{
          "TYPE": "DetachProcessedOrders",
          "orderIds": ["ORDER"]
        }""")

    "KillProcess" in:
      testJson[SubagentCommand](
        KillProcess(OrderId("ORDER"), SIGTERM),
        json"""{
          "TYPE": "KillProcess",
          "orderId": "ORDER",
          "signal": "SIGTERM"
        }""")
  }
