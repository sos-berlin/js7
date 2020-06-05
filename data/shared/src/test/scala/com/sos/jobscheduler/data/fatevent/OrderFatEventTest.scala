package js7.data.fatevent

import js7.base.circeutils.CirceUtils._
import js7.base.time.Timestamp
import js7.base.web.Uri
import js7.data.agent.AgentRefPath
import js7.data.fatevent.OrderFatEvent._
import js7.data.job.ReturnCode
import js7.data.order.{OrderId, Outcome}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OrderFatEventTest extends AnyFreeSpec {

  "OrderAddedFat" in {
    testJson[OrderFatEvent](
      OrderAddedFat(
        (WorkflowPath("/WORKFLOW") ~ "VERSION") /: Position(0),
        Some(Timestamp.ofEpochMilli(111)),
        Map("KEY" -> "VALUE")),
      json"""{
        "TYPE": "OrderAddedFat",
        "workflowPosition": {
          "workflowId": {
            "path": "/WORKFLOW",
            "versionId": "VERSION"
          },
          "position": [ 0 ]
        },
        "scheduledFor": 111,
        "arguments": {
          "KEY": "VALUE"
        }
      }""")
  }

  "OrderForkedFat" in {
    testJson[OrderFatEvent](
      OrderForkedFat(
        (WorkflowPath("/WORKFLOW") ~ "VERSION") /: Position(0),
        List(
          OrderForkedFat.Child("A", OrderId("ORDER-ID/A"), Map("KEY" -> "VALUE")),
          OrderForkedFat.Child("B", OrderId("ORDER-ID/B"), Map.empty))),
      json"""
      {
        "TYPE": "OrderForkedFat",
        "workflowPosition": {
          "workflowId": {
            "path": "/WORKFLOW",
            "versionId": "VERSION"
          },
          "position": [ 0 ]
        },
        "children": [
          {
            "branchId": "A",
            "orderId": "ORDER-ID/A",
            "arguments": {
              "KEY": "VALUE"
            }
          }, {
            "branchId": "B",
            "orderId": "ORDER-ID/B",
            "arguments": {}
          }
        ]
      }""")
  }

  "OrderJoinedFat" in {
    testJson[OrderFatEvent](
      OrderJoinedFat(
        OrderId("A/1") :: OrderId("B/1") :: Nil,
        Outcome.Succeeded(Map("KEY" -> "VALUE")),
        ),
      json"""
      {
        "TYPE": "OrderJoinedFat",
        "childOrderIds": [
          "A/1",
          "B/1"
        ],
        "outcome": {
          "TYPE": "Succeeded",
          "returnCode": 0,
          "keyValues": {
            "KEY": "VALUE"
          }
        }
      }""")
  }

  "OrderFinishedFat" in {
    testJson[OrderFatEvent](
      OrderFinishedFat(
        (WorkflowPath("/WORKFLOW") ~ "VERSION") /: Position(99)),
      json"""{
        "TYPE": "OrderFinishedFat",
        "workflowPosition": {
          "workflowId": {
            "path": "/WORKFLOW",
            "versionId": "VERSION"
          },
          "position": [ 99 ]
        }
      }"""
    )
  }

  "OrderFailedFat" in {
    testJson[OrderFatEvent](
      OrderFailedFat(
        (WorkflowPath("/WORKFLOW") ~ "VERSION") /: Position(99),
        Outcome.Failed(Some("ERROR MESSAGE"), ReturnCode(1), Map.empty)),
      json"""{
        "TYPE": "OrderFailedFat",
        "workflowPosition": {
          "workflowId": {
            "path": "/WORKFLOW",
            "versionId": "VERSION"
          },
          "position": [ 99 ]
        },
        "outcome": {
          "TYPE": "Failed",
          "message": "ERROR MESSAGE",
          "returnCode": 1
        }
      }"""
    )
  }

  "OrderCancelledFat" in {
    testJson[OrderFatEvent](
      OrderCancelledFat(
        (WorkflowPath("/WORKFLOW") ~ "VERSION") /: Position(99)),
      json"""{
        "TYPE": "OrderCancelledFat",
        "workflowPosition": {
          "workflowId": {
            "path": "/WORKFLOW",
            "versionId": "VERSION"
          },
          "position": [ 99 ]
        }
      }"""
    )
  }

  "OrderProcessingStartedFat" in {
    testJson[OrderFatEvent](
      OrderProcessingStartedFat(
        (WorkflowPath("/WORKFLOW") ~ "VERSION") /: Position(0),
        AgentRefPath("/AGENT"),
        Uri("https://agent-1"),
        Some(WorkflowJob.Name("JOB")),
        Map("KEY" -> "VALUE")),
      json"""{
        "TYPE": "OrderProcessingStartedFat",
        "workflowPosition": {
          "workflowId": {
            "path": "/WORKFLOW",
            "versionId": "VERSION"
          },
          "position": [ 0 ]
        },
        "agentRefPath": "/AGENT",
        "agentUri": "https://agent-1",
        "jobName": "JOB",
        "keyValues": {
          "KEY": "VALUE"
        }
      }"""
    )
  }

  "OrderProcessedFat" in {
    testJson[OrderFatEvent](
      OrderProcessedFat(
        Outcome.succeeded,
        Map("KEY" -> "VALUE")),
      json"""{
        "TYPE": "OrderProcessedFat",
        "outcome": {
          "TYPE": "Succeeded",
          "returnCode": 0
        },
        "keyValues": {
          "KEY": "VALUE"
        }
      }"""
    )
  }
}
