package com.sos.jobscheduler.data.fatevent

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.fatevent.OrderFatEvent._
import com.sos.jobscheduler.data.job.{JobPath, ReturnCode}
import com.sos.jobscheduler.data.order.{OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.{Position, WorkflowPath}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OrderFatEventTest extends FreeSpec {

  "OrderAddedFat" in {
    testJson[OrderFatEvent](
      OrderAddedFat(
        (WorkflowPath("/WORKFLOW") % "VERSION") /: Position(0),
        Some(Timestamp.ofEpochMilli(111)),
        Map("VARIABLE" → "VALUE")),
      json"""{
        "TYPE": "OrderAddedFat",
        "workflowPosition": {
          "workflowId": {
            "path": "/WORKFLOW",
            "versionId": "VERSION"
          },
          "position": [ 0 ]
        },
        "scheduledAt": 111,
        "variables": {
          "VARIABLE": "VALUE"
        }
      }""")
  }

  "OrderForkedFat" in {
    testJson[OrderFatEvent](
      OrderForkedFat(
        (WorkflowPath("/WORKFLOW") % "VERSION") /: Position(0),
        List(
          OrderForkedFat.Child("A", OrderId("ORDER-ID/A"), Map("KEY" → "VALUE")),
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
            "variables": {
              "KEY": "VALUE"
            }
          }, {
            "branchId": "B",
            "orderId": "ORDER-ID/B",
            "variables": {}
          }
        ]
      }""")
  }

  "OrderJoinedFat" in {
    testJson[OrderFatEvent](
      OrderJoinedFat(
        OrderId("A/1") :: OrderId("A/2") :: Nil,
        Map("KEY" → "VALUE"), Outcome.Undisrupted(ReturnCode(0), success = true)),
      json"""
      {
        "TYPE": "OrderJoinedFat",
        "childOrderIds": [
          "A/1",
          "A/2"
        ],
        "outcome": {
          "TYPE": "Succeeded",
          "returnCode": 0
        },
        "variables": {
          "KEY": "VALUE"
        }
      }""")
  }

  "OrderFinishedFat" in {
    testJson[OrderFatEvent](
      OrderFinishedFat(
        (WorkflowPath("/WORKFLOW") % "VERSION") /: Position(99)),
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

  "OrderProcessingStartedFat" in {
    testJson[OrderFatEvent](
      OrderProcessingStartedFat(
        (WorkflowPath("/WORKFLOW") % "VERSION") /: Position(0),
        AgentPath("/AGENT"),
        "https://agent-1",
        JobPath("/JOB"),
        Map("KEY" → "VALUE")),
      json"""{
        "TYPE": "OrderProcessingStartedFat",
        "workflowPosition": {
          "workflowId": {
            "path": "/WORKFLOW",
            "versionId": "VERSION"
          },
          "position": [ 0 ]
        },
        "agentPath": "/AGENT",
        "agentUri": "https://agent-1",
        "jobPath": "/JOB",
        "variables": {
          "KEY": "VALUE"
        }
      }"""
    )
  }

  "OrderProcessedFat" in {
    testJson[OrderFatEvent](
      OrderProcessedFat(
        Outcome.succeeded,
        Map("KEY" → "VALUE")),
      json"""{
        "TYPE": "OrderProcessedFat",
        "outcome": {
          "TYPE": "Succeeded",
          "returnCode": 0
        },
        "variables": {
          "KEY": "VALUE"
        }
      }"""
    )
  }
}
