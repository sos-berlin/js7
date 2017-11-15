package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.jobnet.{JobnetPath, NodeId, NodeKey}
import com.sos.jobscheduler.data.order.OrderEvent._
import org.scalatest.FreeSpec
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class OrderEventTest extends FreeSpec {

  "OrderAdded" in {
    check(OrderAdded(NodeKey(JobnetPath("/JOBNET"), NodeId("NODE-ID")), Order.Waiting, Map("VAR" → "VALUE"), Order.Good(true)),
      """{
        "TYPE":"OrderAdded",
        "nodeKey": {
          "jobnetPath": "/JOBNET",
          "nodeId": "NODE-ID"
        },
        "state": {
          "TYPE":"Waiting"
        },
        "variables": {
          "VAR": "VALUE"
        },
        "outcome": {
          "returnValue": true
        }
      }""")
  }

  "OrderAttached" in {
    check(OrderAttached(NodeKey(JobnetPath("/JOBNET"), NodeId("NODE-ID")), Order.Waiting, Map("VAR" → "VALUE"), Order.Good(true)),
      """{
        "TYPE": "OrderAttached",
        "nodeKey": {
          "jobnetPath": "/JOBNET",
          "nodeId": "NODE-ID"
        },
        "state": {
          "TYPE":"Waiting"
        },
        "variables": {
          "VAR": "VALUE"
        },
        "outcome": {
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

  "OrderStepStarted" in {
    check(OrderStepStarted,
      """{
        "TYPE": "OrderStepStarted"
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

  "OrderStepSucceeded" in {
    check(OrderStepSucceeded(MapDiff(addedOrUpdated = Map("VAR" → "VALUE"), removed = Set("REMOVED")), true, NodeId("NEXT")),
      """{
        "TYPE": "OrderStepSucceeded",
        "variablesDiff": {
          "addedOrUpdated": {
            "VAR": "VALUE"
          },
          "removed": ["REMOVED"]
        },
        "returnValue": true,
        "nextNodeId": "NEXT"
      }""")
  }

  "OrderStepFailed" in {
    check(OrderStepFailed(OrderStepFailed.Other("ERROR"), NodeId("NEXT")),
      """{
        "TYPE": "OrderStepFailed",
        "reason": {
          "TYPE": "Other",
          "message": "ERROR"
        },
        "nextNodeId": "NEXT"
      }""")
  }

  "OrderStepFailed(AgentAborted)" in {
    check(OrderStepFailed(OrderStepFailed.AgentAborted, NodeId("NEXT")),
      """{
        "TYPE": "OrderStepFailed",
        "reason": {
          "TYPE": "AgentAborted"
        },
        "nextNodeId": "NEXT"
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
    val jsValue = json.parseJson
    assert(event.toJson == jsValue)
    assert(event == jsValue.convertTo[OrderEvent])
  }
}
