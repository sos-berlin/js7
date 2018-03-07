package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderEvent._
import com.sos.jobscheduler.data.workflow.{Position, WorkflowPath}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.Json
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OrderEventTest extends FreeSpec {

  "OrderAdded" in {
    check(OrderAdded(WorkflowPath("/WORKFLOW") % "VERSION", Order.StartNow, Payload(Map("VAR" → "VALUE"))), json"""
      {
        "TYPE": "OrderAdded",
        "workflowId": {
          "path": "/WORKFLOW",
          "versionId": "VERSION"
        },
        "state": {
          "TYPE": "StartNow"
        },
        "payload": {
          "variables": {
            "VAR": "VALUE"
          }
        }
      }""")
  }

  "OrderAttached" in {
    check(
      OrderAttached(
        (WorkflowPath("/WORKFLOW") % "VERSION") /: Position(2),
        Order.Ready, Some(OrderId("PARENT")), AgentPath("/AGENT"), Payload(Map("VAR" → "VALUE"))),
      json"""{
        "TYPE": "OrderAttached",
        "workflowPosition": {
          "workflowId": {
            "path": "/WORKFLOW",
            "versionId": "VERSION"
          },
          "position": [ 2 ]
        },
        "state": {
          "TYPE": "Ready"
        },
        "parent": "PARENT",
        "agentPath": "/AGENT",
        "payload": {
          "variables": {
            "VAR": "VALUE"
          }
        }
      }""")
  }

  "OrderTransferredToAgent" in {
    check(OrderTransferredToAgent(AgentPath("/AGENT")), json"""
      {
        "TYPE": "OrderTransferredToAgent",
        "agentPath": "/AGENT"
      }""")
  }

  "OrderProcessingStarted" in {
    check(OrderProcessingStarted, json"""
      {
        "TYPE": "OrderProcessingStarted"
      }""")
  }

  "OrderStdoutWritten toString" in {
    assert(OrderStderrWritten("*" * 100).toString ==
      "OrderStderrWritten(*****************************************************************...(length 100))")
  }

  "OrderStdoutWritten" in {
    check(OrderStdoutWritten("STDOUT\n"), json"""
      {
        "TYPE": "OrderStdoutWritten",
        "chunk": "STDOUT\n"
      }""")
  }

  "OrderStderrWritten" in {
    check(OrderStderrWritten("STDOUT\n"), json"""
      {
        "TYPE": "OrderStderrWritten",
        "chunk": "STDOUT\n"
      }""")
  }

  "OrderProcessed" in {
    check(OrderProcessed(MapDiff(changed = Map("VAR" → "VALUE"), deleted = Set("REMOVED")), Outcome.Succeeded(ReturnCode(0))), json"""
      {
        "TYPE": "OrderProcessed",
        "variablesDiff": {
          "changed": {
            "VAR": "VALUE"
          },
          "deleted": ["REMOVED"]
        },
        "outcome": {
          "TYPE": "Succeeded",
          "returnCode": 0
        }
      }""")
  }

  "OrderForked" in {
    check(OrderForked(List(
      OrderForked.Child("A", OrderId("ORDER-ID/A"), MapDiff(Map("CHANGED" → "x"))),
      OrderForked.Child("B", OrderId("ORDER-ID/B")))), json"""
      {
        "TYPE": "OrderForked",
        "children": [
          {
            "branchId": "A",
            "orderId": "ORDER-ID/A",
            "variablesDiff": {
              "changed": { "CHANGED": "x" },
              "deleted": []
            }
          }, {
            "branchId": "B",
            "orderId": "ORDER-ID/B",
            "variablesDiff": {
              "changed": {},
              "deleted": []
            }
          }
        ]
      }""")
  }

  "OrderJoined" in {
    check(OrderJoined(MapDiff.empty, Outcome.succeeded), json"""
      {
        "TYPE": "OrderJoined",
        "variablesDiff": {
          "changed": {},
          "deleted": []
        },
        "outcome": {
          "TYPE": "Succeeded",
          "returnCode": 0
        }
      }""")
  }

  "OrderOffered" in {
    check(OrderOffered(OrderId("ORDER-ID"), Timestamp.ofEpochMilli(123)), json"""
      {
        "TYPE": "OrderOffered",
        "orderId": "ORDER-ID",
        "until": 123
      }""")
  }

  "OrderAwaiting" in {
    check(OrderAwaiting(OrderId("ORDER-ID")), json"""
      {
        "TYPE": "OrderAwaiting",
        "orderId": "ORDER-ID"
      }""")
  }

  "OrderMoved" in {
    check(OrderMoved(Position(7)), json"""
      {
        "TYPE": "OrderMoved",
        "to": [ 7 ]
      }""")
  }

  "OrderDetachable" in {
    check(OrderDetachable, json"""
      {
        "TYPE": "OrderDetachable"
      }""")
  }

  "OrderDetached" in {
    check(OrderDetached, json"""
      {
        "TYPE": "OrderDetached"
      }""")
  }

  "OrderFinished" in {
    check(OrderFinished, json"""
      {
        "TYPE": "OrderFinished"
      }""")
  }

  private def check(event: OrderEvent, json: ⇒ Json) = testJson(event, json)

  if (sys.props contains "test.speed") "Speed" in {
    val n = 10000
    val event = Stamped(12345678, Timestamp.ofEpochMilli(1),
      KeyedEvent[OrderEvent](OrderId("ORDER"), OrderAdded(WorkflowPath("/WORKFLOW") % "VERSION", Order.Ready, Payload(Map("VAR" → "VALUE")))))
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
