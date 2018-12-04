package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderEvent._
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.Json
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OrderEventTest extends FreeSpec {

  "OrderAdded" in {
    check(OrderAdded(WorkflowPath("/WORKFLOW") % "VERSION", None, Payload(Map("VAR" → "VALUE"))), json"""
      {
        "TYPE": "OrderAdded",
        "workflowId": {
          "path": "/WORKFLOW",
          "versionId": "VERSION"
        },
        "variables": {
          "VAR": "VALUE"
        }
      }""")
  }

  "OrderAttachable" in {
    check(
      OrderAttachable(AgentPath("/AGENT")),
      json"""{
        "TYPE": "OrderAttachable",
        "agentPath": "/AGENT"
      }""")
  }

  "OrderAttached" in {
    check(
      OrderAttached(
        (WorkflowPath("/WORKFLOW") % "VERSION") /: Position(2),
        Order.Ready, Some(OrderId("PARENT")), AgentPath("/AGENT") % "1", Payload(Map("VAR" → "VALUE"))),
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
        "agentId": {
          "path": "/AGENT",
          "versionId": "1"
        },
        "payload": {
          "variables": {
            "VAR": "VALUE"
          }
        }
      }""")
  }

  "OrderTransferredToAgent" in {
    check(OrderTransferredToAgent(AgentPath("/AGENT") % "1"), json"""
      {
        "TYPE": "OrderTransferredToAgent",
        "agentId": {
          "path": "/AGENT",
          "versionId": "1"
        }
      }""")
  }

  "OrderStarted" in {
    check(OrderStarted, json"""
      {
        "TYPE": "OrderStarted"
      }""")
  }

  "OrderProcessingStarted" in {
    check(OrderProcessingStarted, json"""
      {
        "TYPE": "OrderProcessingStarted"
      }""")
  }

  "OrderStdoutWritten toString" in {
    assert(OrderStderrWritten("*"*30 + "\r\n" + "*"*70).toString ==
      """OrderStderrWritten(******************************\r\n*********************************...(length 102))""")
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

  "OrderStopped(Failed)" in {
    check(OrderStopped(Outcome.Failed(ReturnCode(1))), json"""
      {
        "TYPE": "OrderStopped",
        "outcome": {
          "TYPE": "Failed",
          "returnCode": 1
        }
      }""")
  }

  "OrderStopped(Disrupted(PROBLEM))" in {
    check(OrderStopped(Outcome.Disrupted(Problem("PROBLEM"))), json"""
      {
        "TYPE": "OrderStopped",
        "outcome": {
          "TYPE": "Disrupted",
          "reason": {
            "TYPE": "Other",
            "problem": {
              "message": "PROBLEM"
            }
          }
        }
      }""")
  }

  "OrderForked" in {
    check(OrderForked(List(
      OrderForked.Child("A", OrderId("A/1"), MapDiff(Map("CHANGED" → "x"))),
      OrderForked.Child("B", OrderId("B/1")))), json"""
      {
        "TYPE": "OrderForked",
        "children": [
          {
            "branchId": "A",
            "orderId": "A/1",
            "variablesDiff": {
              "changed": { "CHANGED": "x" },
              "deleted": []
            }
          }, {
            "branchId": "B",
            "orderId": "B/1",
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

  "OrderCancelationMarked" in {
    check(OrderCancelationMarked(CancelMode.NotStarted), json"""
      {
        "TYPE": "OrderCancelationMarked",
        "mode": {
          "TYPE": "NotStarted"
        }
      }""")
  }

  "OrderCanceled" in {
    check(OrderCanceled, json"""
      {
        "TYPE": "OrderCanceled"
      }""")
  }

  private def check(event: OrderEvent, json: ⇒ Json) = testJson(event, json)

  if (sys.props contains "test.speed") "Speed" in {
    val n = 10000
    val event = Stamped(12345678, Timestamp.ofEpochMilli(1),
      KeyedEvent[OrderEvent](OrderId("ORDER"), OrderAdded(WorkflowPath("/WORKFLOW") % "VERSION", payload = Payload(Map("VAR" → "VALUE")))))
    val jsonString = event.asJson.compactPrint
    println(f"${"Serialize"}%-20s Deserialize")
    for (_ ← 1 to 10) {
      val circeSerialize = measure(event.asJson.compactPrint)
      val circeDeserialize = measure(jsonString.parseJson.as[OrderEvent].orThrow: OrderEvent)
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
