package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent._
import com.sos.jobscheduler.data.workflow.{NodeId, NodeKey, WorkflowPath}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OrderEventTest extends FreeSpec {

  "OrderAdded" in {
    check(OrderAdded(NodeKey(WorkflowPath("/JOBNET"), NodeId("NODE-ID")), Order.Ready, Payload(Map("VAR" → "VALUE"))),
      """{
        "TYPE":"OrderAdded",
        "nodeKey": {
          "workflowPath": "/JOBNET",
          "nodeId": "NODE-ID"
        },
        "state": {
          "TYPE":"Ready"
        },
        "payload": {
          "variables": {
            "VAR": "VALUE"
          },
          "outcome": {
            "TYPE": "Good",
            "returnValue": true
          }
        }
      }""")
  }

  "OrderAttached" in {
    check(OrderAttached(NodeKey(WorkflowPath("/JOBNET"), NodeId("NODE-ID")), Order.Ready, Some(OrderId("PARENT")), AgentPath("/AGENT"), Payload(Map("VAR" → "VALUE"))),
      """{
        "TYPE": "OrderAttached",
        "nodeKey": {
          "workflowPath": "/JOBNET",
          "nodeId": "NODE-ID"
        },
        "state": {
          "TYPE":"Ready"
        },
        "parent": "PARENT",
        "agentPath": "/AGENT",
        "payload": {
          "variables": {
            "VAR": "VALUE"
          },
          "outcome": {
            "TYPE": "Good",
            "returnValue": true
          }
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

  "OrderStdoutWritten toString" in {
    assert(OrderStderrWritten("*" * 100).toString ==
      "OrderStderrWritten(*****************************************************************...(length 100))")
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
    check(OrderProcessed(MapDiff(addedOrUpdated = Map("VAR" → "VALUE"), removed = Set("REMOVED")), Outcome.Good(true)),
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

  "OrderForked" in {
    check(OrderForked(List(
      OrderForked.Child(OrderId("ORDER-ID/A"), NodeId("A"), Payload.empty),
      OrderForked.Child(OrderId("ORDER-ID/B"), NodeId("B"), Payload.empty))),
      """{
        "TYPE": "OrderForked",
        "children": [
          {
            "orderId": "ORDER-ID/A",
            "nodeId": "A",
            "payload": {
              "variables": {},
              "outcome": {
                "TYPE": "Good",
                "returnValue": true
              }
            }
          }, {
            "orderId": "ORDER-ID/B",
            "nodeId": "B",
            "payload": {
              "variables": {},
              "outcome": {
                "TYPE": "Good",
                "returnValue": true
              }
            }
          }
        ]
      }""")
  }

  "OrderJoined" in {
    check(OrderJoined(NodeId("JOINED"), MapDiff.empty, Outcome.Default),
      """{
        "TYPE": "OrderJoined",
        "toNodeId": "JOINED",
        "variablesDiff": {
          "addedOrUpdated": {},
          "removed": []
        },
        "outcome": {
          "TYPE": "Good",
          "returnValue": true
        }
      }""")
  }

  "OrderMoved" in {
    check(OrderMoved(NodeId("NODE")),
      """{
        "TYPE": "OrderMoved",
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

  private def check(event: OrderEvent, json: String) = testJson(event, json)

  if (sys.props contains "test.speed") "Speed" in {
    val n = 10000
    val event = Stamped(12345678, KeyedEvent[OrderEvent](OrderId("ORDER"), OrderAdded(NodeKey(WorkflowPath("/JOBNET"), NodeId("NODE-ID")), Order.Ready, Payload(Map("VAR" → "VALUE")))))
    val jsonString = event.asJson.compactPrint
    println(f"${"Serialize"}%-20s Deserialize")
    for (_ ← 1 to 10) {
      val circeSerialize = measure(event.asJson.compactPrint)
      val circeDeserialize = measure(jsonString.parseJson.as[OrderEvent].force: OrderEvent)
      println(f"$circeSerialize%-20s $circeDeserialize%-20s")
    }
    def measure[A](serialize: ⇒ Unit) = {
      val t = System.currentTimeMillis
      for (_ ← 1 to n) serialize
      val d = System.currentTimeMillis - t
      s"${d}ms ${n*1000/d}/s"
    }
  }
}
