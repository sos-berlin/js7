package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent._
import com.sos.jobscheduler.data.workflow.{Position, WorkflowPath}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OrderEventTest extends FreeSpec {

  "OrderAdded" in {
    check(OrderAdded(WorkflowPath("/JOBNET"), Order.Ready, Payload(Map("VAR" → "VALUE"))),
      """{
        "TYPE": "OrderAdded",
        "workflowPath": "/JOBNET",
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
    check(OrderAttached(WorkflowPath("/JOBNET") /: Position(2), Order.Ready, Some(OrderId("PARENT")), AgentPath("/AGENT"), Payload(Map("VAR" → "VALUE"))),
      """{
        "TYPE": "OrderAttached",
        "workflowPosition": [ "/JOBNET", 2 ],
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
      OrderForked.Child(OrderId.ChildId("A"), OrderId("ORDER-ID/A"), MapDiff(Map("added" → "x"))),
      OrderForked.Child(OrderId.ChildId("B"), OrderId("ORDER-ID/B")))),
      """{
        "TYPE": "OrderForked",
        "children": [
          {
            "childId": "A",
            "orderId": "ORDER-ID/A",
            "variablesDiff": {
              "addedOrUpdated": { "added": "x" },
              "removed": []
            }
          }, {
            "childId": "B",
            "orderId": "ORDER-ID/B",
            "variablesDiff": {
              "addedOrUpdated": {},
              "removed": []
            }
          }
        ]
      }""")
  }

  "OrderJoined" in {
    check(OrderJoined(7, MapDiff.empty, Outcome.Default),
      """{
        "TYPE": "OrderJoined",
        "to": 7,
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
    check(OrderMoved(7),
      """{
        "TYPE": "OrderMoved",
        "to": 7
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
    val event = Stamped(12345678, KeyedEvent[OrderEvent](OrderId("ORDER"), OrderAdded(WorkflowPath("/JOBNET"), Order.Ready, Payload(Map("VAR" → "VALUE")))))
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
