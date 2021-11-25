package js7.subagent.data

import js7.agent.data.AgentState
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.crypt.silly.SillySigner
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.item.ItemSigner
import js7.data.order.{Order, OrderId}
import js7.data.subagent.SubagentId
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.subagent.data.SubagentCommand.{AttachItem, AttachSignedItem, DedicateSubagent, KillProcess, ShutDown, StartOrderProcess}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class SubagentCommandTest extends AnyFreeSpec
{
  "JSON" - {
    "DedicateSubagent" in {
      testJson[SubagentCommand](
        DedicateSubagent(
          SubagentId("SUBAGENT"),
          AgentPath("AGENT"),
          ControllerId("CONTROLLER")),
        json"""{
          "TYPE": "DedicateSubagent",
          "subagentId": "SUBAGENT",
          "agentPath": "AGENT",
          "controllerId": "CONTROLLER"
        }""")
    }

    //"AttachUnsignedItem" in {
    //  testJson[SubagentCommand](
    //    AttachUnsignedItem(xx),
    //    json"""{
    //      "TYPE": "AttachUnignedItem"
    //    } """)
    //}

    "AttachSignedItem" in {
      val itemSigner = new ItemSigner(SillySigner.Default, AgentState.signableItemJsonCodec)
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
    }

    "AttachItem" in {
      testJson[SubagentCommand](
        AttachItem(Workflow(WorkflowPath("WORKFLOW") ~ "1", Nil)),
        json"""{
          "TYPE": "AttachItem",
          "item": {
            "TYPE": "Workflow",
            "path": "WORKFLOW",
            "versionId": "1",
            "instructions": []
          }
        }""")
    }

    "ShutDown" in {
      testJson[SubagentCommand](
        ShutDown(Some(SIGTERM), restart = true),
        json"""{
          "TYPE": "ShutDown",
          "processSignal": "SIGTERM",
          "restart": true
        } """)
    }

    "StartOrderProcess" in {
      testJson[SubagentCommand](
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
    }

    "KillProcess" in {
      testJson[SubagentCommand](
        KillProcess(OrderId("ORDER"), SIGTERM),
        json"""{
          "TYPE": "KillProcess",
          "orderId": "ORDER",
          "signal": "SIGTERM"
        }""")
    }
  }
}
