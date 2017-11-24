package com.sos.jobscheduler.master.gui.data

import com.sos.jobscheduler.master.gui.data.OrderEvent._
import io.circe.parser.parse
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
        },
        "nextNodeId": "NEXT"
      }""")
  }

  "OrderTransitioned" in {
    check(OrderTransitioned(NodeId("NEXT")),
      """{
        "TYPE": "OrderTransitioned",
        "toNodeId": "NEXT"
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

  private def check(event: OrderEvent, json: String) = {
    //assert(event.asJson == parse(json).toTry.get)
    assert(parse(json).toTry.get.as[OrderEvent] == Right(event))
  }
}
