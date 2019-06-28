package com.sos.jobscheduler.data.fatevent

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.fatevent.OrderFatEvent._
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.{OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OrderFatEventTest extends FreeSpec {

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

  "OrderStoppedFat" in {
    testJson[OrderFatEvent](
      OrderStoppedFat(
        (WorkflowPath("/WORKFLOW") ~ "VERSION") /: Position(99),
        Outcome.Failed(Some("ERROR MESSAGE"), ReturnCode(1), Map.empty)),
      json"""{
        "TYPE": "OrderStoppedFat",
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

  "OrderCanceledFat" in {
    testJson[OrderFatEvent](
      OrderCanceledFat(
        (WorkflowPath("/WORKFLOW") ~ "VERSION") /: Position(99)),
      json"""{
        "TYPE": "OrderCanceledFat",
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
        "https://agent-1",
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
