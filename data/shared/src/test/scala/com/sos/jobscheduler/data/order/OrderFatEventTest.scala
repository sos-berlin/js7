package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.order.OrderFatEvent.{OrderAddedFat, OrderFinishedFat, OrderForkedFat, OrderProcessedFat, OrderProcessingStartedFat}
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
