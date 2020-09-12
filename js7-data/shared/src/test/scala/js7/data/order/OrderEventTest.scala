package js7.data.order

import io.circe.Json
import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentRefPath
import js7.data.command.CancelMode
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.job.ReturnCode
import js7.data.order.OrderEvent._
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class OrderEventTest extends AnyFreeSpec {

  "OrderAdded" in {
    check(OrderAdded(WorkflowPath("/WORKFLOW") ~ "VERSION", None, Map("VAR" -> "VALUE")), json"""
      {
        "TYPE": "OrderAdded",
        "workflowId": {
          "path": "/WORKFLOW",
          "versionId": "VERSION"
        },
        "arguments": {
          "VAR": "VALUE"
        }
      }""")
  }

  "OrderAttachable" in {
    check(
      OrderAttachable(AgentRefPath("/AGENT")),
      json"""{
        "TYPE": "OrderAttachable",
        "agentRefPath": "/AGENT"
      }""")
  }

  "OrderAttachedToAgent" in {
    check(
      OrderAttachedToAgent(
        (WorkflowPath("/WORKFLOW") ~ "VERSION") /: Position(2),
        Order.Ready,
        Map("KEY" -> "VALUE"),
        HistoricOutcome(Position(123), Outcome.succeeded) :: Nil,
        AgentRefPath("/AGENT"),
        Some(OrderId("PARENT")),
        Some(OrderMark.Suspending),
        isSuspended = true),
      json"""{
        "TYPE": "OrderAttachedToAgent",
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
        "arguments": {
          "KEY": "VALUE"
        },
        "historicOutcomes": [
          {
            "position": [123],
            "outcome": {
              "TYPE": "Succeeded",
              "returnCode": 0
            }
          }
        ],
        "agentRefPath":"/AGENT",
        "parent": "PARENT",
        "mark": { "TYPE": "Suspending" },
        "isSuspended":  true
      }""")
  }

  "OrderAttached" in {
    check(OrderAttached(AgentRefPath("/AGENT")), json"""
      {
        "TYPE": "OrderAttached",
        "agentRefPath":"/AGENT"
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
    check(OrderProcessed(Outcome.Succeeded(Map("KEY" -> "VALUE"))), json"""
      {
        "TYPE": "OrderProcessed",
        "outcome": {
          "TYPE": "Succeeded",
          "returnCode": 0,
          "keyValues": {
            "KEY": "VALUE"
          }
        }
      }""")
  }

  "OrderCatched" in {
    check(OrderCatched(Outcome.Failed(ReturnCode(1)), Position(1)), json"""
      {
        "TYPE": "OrderCatched",
        "outcome": {
          "TYPE": "Failed",
          "returnCode": 1
        },
        "movedTo": [ 1 ]
      }""")
  }

  "OrderCatched complete" in {
    check(OrderCatched(Outcome.Failed(Some("FAILED"), ReturnCode(1)), Position(1)), json"""
      {
        "TYPE": "OrderCatched",
        "outcome": {
          "TYPE": "Failed",
          "returnCode": 1,
          "message": "FAILED"
        },
        "movedTo": [ 1 ]
      }""")
  }

  "OrderFailed" in {
    check(OrderFailed(Outcome.Failed(ReturnCode(1))), json"""
      {
        "TYPE": "OrderFailed",
        "outcome": {
          "TYPE": "Failed",
          "returnCode": 1
        }
      }""")
  }

  "OrderFailed(Failed) complete" in {
    check(OrderFailed(Outcome.Failed(Some("ERROR"), ReturnCode(1))), json"""
      {
        "TYPE": "OrderFailed",
        "outcome": {
          "TYPE": "Failed",
          "returnCode": 1,
          "message": "ERROR"
        }
      }""")
  }

  "OrderFailed(Disrupted(PROBLEM))" in {
    check(OrderFailed(Outcome.Disrupted(Problem("PROBLEM"))), json"""
      {
        "TYPE": "OrderFailed",
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

  "OrderFailedInFork" in {
    check(OrderFailedInFork(Outcome.Failed(ReturnCode(1))), json"""
      {
        "TYPE": "OrderFailedInFork",
        "outcome": {
          "TYPE": "Failed",
          "returnCode": 1
        }
      }""")
  }

  "OrderFailedInFork complete" in {
    check(OrderFailedInFork(Outcome.Failed(Some("ERROR"), ReturnCode(1))), json"""
      {
        "TYPE": "OrderFailedInFork",
        "outcome": {
          "TYPE": "Failed",
          "returnCode": 1,
          "message": "ERROR"
        }
      }""")
  }

  "OrderRetrying" in {
    check(OrderRetrying(Position(1)), json"""
      {
        "TYPE": "OrderRetrying",
        "movedTo": [ 1 ]
      }""")
  }

  "OrderRetrying(delayedUntil)" in {
    check(OrderRetrying(Position(1), Some(Timestamp("2019-03-04T12:00:00Z"))), json"""
      {
        "TYPE": "OrderRetrying",
        "movedTo": [ 1 ],
        "delayedUntil": 1551700800000
      }""")
  }

  "OrderForked" in {
    check(OrderForked(List(
      OrderForked.Child("A", OrderId("A/1")),
      OrderForked.Child("B", OrderId("B/1")))), json"""
      {
        "TYPE": "OrderForked",
        "children": [
          {
            "branchId": "A",
            "orderId": "A/1"
          }, {
            "branchId": "B",
            "orderId": "B/1"
          }
        ]
      }""")
  }

  "OrderJoined" in {
    check(OrderJoined(Outcome.succeeded), json"""
      {
        "TYPE": "OrderJoined",
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

  "OrderCancelMarked" in {
    check(OrderCancelMarked(CancelMode.NotStarted), json"""
      {
        "TYPE": "OrderCancelMarked",
        "mode": {
          "TYPE": "NotStarted"
        }
      }""")
  }

  "OrderCancelled" in {
    check(OrderCancelled, json"""
      {
        "TYPE": "OrderCancelled"
      }""")
  }

  "OrderSuspendMarked" in {
    check(OrderSuspendMarked, json"""
      {
        "TYPE": "OrderSuspendMarked"
      }""")
  }

  "OrderSuspended" in {
    check(OrderSuspended, json"""
      {
        "TYPE": "OrderSuspended"
      }""")
  }

  "OrderResumeMarked" in {
    check(OrderResumeMarked, json"""
      {
        "TYPE": "OrderResumeMarked"
      }""")
  }

  "OrderResumed" in {
    check(OrderResumed, json"""
      {
        "TYPE": "OrderResumed"
      }""")
  }

  private def check(event: OrderEvent, json: => Json) = testJson(event, json)

  if (sys.props contains "test.speed") "Speed" in {
    val n = 10000
    val event = Stamped(12345678L, Timestamp.ofEpochMilli(1),
      KeyedEvent[OrderEvent](OrderId("ORDER"), OrderAdded(WorkflowPath("/WORKFLOW") ~ "VERSION", arguments = Map("KEY" -> "VALUE"))))
    val jsonString = event.asJson.compactPrint
    println(f"${"Serialize"}%-20s Deserialize")
    for (_ <- 1 to 10) {
      val circeSerialize =  measure(event.asJson.compactPrint)
      val circeDeserialize = measure(jsonString.parseJsonOrThrow.as[OrderEvent].orThrow: OrderEvent)
      println(f"$circeSerialize%-20s $circeDeserialize%-20s")
    }
    def measure[A](serialize: => Unit) = {
      val t = System.nanoTime
      for (_ <- 1 to n) serialize
      val d = (System.nanoTime - t).nanoseconds
      s"${d.pretty} ${n*1000/d.toMillis}/s"
    }
  }
}
