package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
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
    check(OrderAdded(WorkflowPath("/JOBNET"), Order.StartNow, Payload(Map("VAR" → "VALUE"))), json"""
      {
        "TYPE": "OrderAdded",
        "workflowPath": "/JOBNET",
        "state": {
          "TYPE": "StartNow"
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
    check(OrderAttached(WorkflowPath("/JOBNET") /: Position(2), Order.Ready, Some(OrderId("PARENT")), AgentPath("/AGENT"), Payload(Map("VAR" → "VALUE"))), json"""
      {
        "TYPE": "OrderAttached",
        "workflowPosition": [ "/JOBNET", 2 ],
        "state": {
          "TYPE": "Ready"
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
    check(OrderProcessed(MapDiff(changed = Map("VAR" → "VALUE"), deleted = Set("REMOVED")), Outcome.Good(true)), json"""
      {
        "TYPE": "OrderProcessed",
        "variablesDiff": {
          "changed": {
            "VAR": "VALUE"
          },
          "deleted": ["REMOVED"]
        },
        "outcome": {
          "TYPE": "Good",
          "returnValue": true
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
    check(OrderJoined(Position(7), MapDiff.empty, Outcome.Default), json"""
      {
        "TYPE": "OrderJoined",
        "next": [ 7 ],
        "variablesDiff": {
          "changed": {},
          "deleted": []
        },
        "outcome": {
          "TYPE": "Good",
          "returnValue": true
        }
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
