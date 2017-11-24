package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.OrderEvent._
import com.sos.jobscheduler.data.workflow.{NodeId, NodeKey, WorkflowPath}
import com.sos.jobscheduler.tester.JsonTester.testSprayJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OrderEventTest extends FreeSpec {

  "OrderAdded" in {
    check(OrderAdded(NodeKey(WorkflowPath("/JOBNET"), NodeId("NODE-ID")), Order.Ready, Map("VAR" → "VALUE"), Order.Good(true)),
      """{
        "TYPE":"OrderAdded",
        "nodeKey": {
          "workflowPath": "/JOBNET",
          "nodeId": "NODE-ID"
        },
        "state": {
          "TYPE":"Ready"
        },
        "variables": {
          "VAR": "VALUE"
        },
        "outcome": {
          "TYPE": "Good",
          "returnValue": true
        }
      }""")
  }

  "OrderAttached" in {
    check(OrderAttached(NodeKey(WorkflowPath("/JOBNET"), NodeId("NODE-ID")), Order.Ready, Map("VAR" → "VALUE"), Order.Good(true)),
      """{
        "TYPE": "OrderAttached",
        "nodeKey": {
          "workflowPath": "/JOBNET",
          "nodeId": "NODE-ID"
        },
        "state": {
          "TYPE":"Ready"
        },
        "variables": {
          "VAR": "VALUE"
        },
        "outcome": {
          "TYPE": "Good",
          "returnValue": true
        }
      }""")
  }

  "OrderMovedToAgent" in {
    check(OrderMovedToAgent(AgentPath("/AGENT")),
      """{
        "TYPE": "OrderMovedToAgent",
        "agentPath": "/AGENT"
      }""")
  }

  "OrderProcessingStarted" in {
    check(OrderProcessingStarted,
      """{
        "TYPE": "OrderProcessingStarted"
      }""")
  }

  "OrderStdoutWritten" in {
    check(OrderStdoutWritten("STDOUT\n"),
      """{
        "TYPE": "OrderStdoutWritten",
        "chunk": "STDOUT\n"
      }""")
  }

  "OrderStderrWritten" in {
    check(OrderStderrWritten("STDOUT\n"),
      """{
        "TYPE": "OrderStderrWritten",
        "chunk": "STDOUT\n"
      }""")
  }

  "OrderProcessed" in {
    check(OrderProcessed(MapDiff(addedOrUpdated = Map("VAR" → "VALUE"), removed = Set("REMOVED")), Order.Good(true)),
      """{
        "TYPE": "OrderProcessed",
        "variablesDiff": {
          "addedOrUpdated": {
            "VAR": "VALUE"
          },
          "removed": ["REMOVED"]
        },
        "outcome": {
          "TYPE": "Good",
          "returnValue": true
        }
      }""")
  }

  "OrderTransitioned" in {
    check(OrderTransitioned(NodeId("NODE")),
      """{
        "TYPE": "OrderTransitioned",
        "toNodeId": "NODE"
      }""")
  }

  "OrderDetachable" in {
    check(OrderDetachable,
      """{
        "TYPE": "OrderDetachable"
      }""")
  }

  "OrderDetached" in {
    check(OrderDetached,
      """{
        "TYPE": "OrderDetached"
      }""")
  }

  "OrderFinished" in {
    check(OrderFinished,
      """{
        "TYPE": "OrderFinished"
      }""")
  }

  private def check(event: OrderEvent, json: String) = testSprayJson(event, json)
}
