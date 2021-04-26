package js7.data.order

import io.circe.Json
import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentPath
import js7.data.command.CancelMode
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.lock.LockPath
import js7.data.order.OrderEvent._
import js7.data.orderwatch.{ExternalOrderKey, ExternalOrderName, OrderWatchPath}
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class OrderEventTest extends AnyFreeSpec
{
  "OrderAdded" in {
    check(
      OrderAdded(
        WorkflowPath("WORKFLOW") ~ "VERSION",
        Map("VAR" -> StringValue("VALUE")),
        Some(Timestamp("2021-01-01T00:00:00Z")),
        Some(ExternalOrderKey(OrderWatchPath("ORDER-WATCH"), ExternalOrderName("ORDER-NAME")))),
      json"""
      {
        "TYPE": "OrderAdded",
        "workflowId": {
          "path": "WORKFLOW",
          "versionId": "VERSION"
        },
        "scheduledFor": 1609459200000,
        "arguments": {
          "VAR": "VALUE"
        },
        "externalOrderKey": {
          "orderWatchPath": "ORDER-WATCH",
          "name": "ORDER-NAME"
        }
      }""")
  }

  "OrderAttachable" in {
    check(
      OrderAttachable(AgentPath("AGENT")),
      json"""{
        "TYPE": "OrderAttachable",
        "agentPath": "AGENT"
      }""")
  }

  "OrderAttachedToAgent" in {
    check(
      OrderAttachedToAgent(
        (WorkflowPath("WORKFLOW") ~ "VERSION") /: Position(2),
        Order.Ready,
        Map("KEY" -> StringValue("VALUE")),
        Some(Timestamp("2017-11-15T12:33:44.789Z")),
        Some(ExternalOrderKey(OrderWatchPath("ORDER-WATCH"), ExternalOrderName("ORDER-NAME"))),
        HistoricOutcome(Position(123), Outcome.succeeded) :: Nil,
        AgentPath("AGENT"),
        Some(OrderId("PARENT")),
        Some(OrderMark.Suspending()),
        isSuspended = true,
        removeWhenTerminated = true),
      json"""{
        "TYPE": "OrderAttachedToAgent",
        "workflowPosition": {
          "workflowId": {
            "path": "WORKFLOW",
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
        "scheduledFor": 1510749224789,
        "externalOrderKey": {
          "orderWatchPath": "ORDER-WATCH",
          "name": "ORDER-NAME"
        },
        "historicOutcomes": [
          {
            "position": [123],
            "outcome": {
              "TYPE": "Succeeded"
            }
          }
        ],
        "agentPath":"AGENT",
        "parent": "PARENT",
        "mark": {
          "TYPE": "Suspending",
          "mode": {}
        },
        "isSuspended":  true,
        "removeWhenTerminated":  true
      }""")
  }

  "OrderAttached" in {
    check(OrderAttached(AgentPath("AGENT")), json"""
      {
        "TYPE": "OrderAttached",
        "agentPath":"AGENT"
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
    check(OrderProcessed(Outcome.Succeeded(Map("KEY" -> StringValue("VALUE")))), json"""
      {
        "TYPE": "OrderProcessed",
        "outcome": {
          "TYPE": "Succeeded",
          "namedValues": {
            "KEY": "VALUE"
          }
        }
      }""")
  }

  "OrderCatched" in {
    check(OrderCatched(Position(1), Some(Outcome.Failed(NamedValues.rc(1)))), json"""
      {
        "TYPE": "OrderCatched",
        "movedTo": [ 1 ],
        "outcome": {
          "TYPE": "Failed",
          "namedValues": {
            "returnCode": 1
          }
        },
        "lockPaths": []
      }""")
  }

  "OrderCatched complete" in {
    check(OrderCatched(Position(1), Some(Outcome.Failed(Some("FAILED"), NamedValues.rc(1))), Seq(LockPath("LOCK"))), json"""
      {
        "TYPE": "OrderCatched",
        "outcome": {
          "TYPE": "Failed",
          "namedValues": {
            "returnCode": 1
          },
          "message": "FAILED"
        },
        "movedTo": [ 1 ],
        "lockPaths": [ "LOCK" ]
      }""")
  }

  "OrderFailed" in {
    check(OrderFailed(Position(1), Some(Outcome.Failed(NamedValues.rc(1)))), json"""
      {
        "TYPE": "OrderFailed",
        "movedTo": [ 1 ],
        "outcome": {
          "TYPE": "Failed",
          "namedValues": {
            "returnCode": 1
          }
        },
        "lockPaths": []
      }""")
  }

  "OrderFailed(Failed) complete" in {
    check(OrderFailed(Position(1), Some(Outcome.Failed(Some("ERROR"), NamedValues.rc(1))), Seq(LockPath("LOCK"))), json"""
      {
        "TYPE": "OrderFailed",
        "movedTo": [ 1 ],
        "outcome": {
          "TYPE": "Failed",
          "namedValues": {
            "returnCode": 1
          },
          "message": "ERROR"
        },
        "lockPaths": [ "LOCK" ]
      }""")
  }

  "OrderFailed(Disrupted(PROBLEM))" in {
    check(OrderFailed(Position(1), Some(Outcome.Disrupted(Problem("PROBLEM")))), json"""
      {
        "TYPE": "OrderFailed",
        "movedTo": [ 1 ],
        "outcome": {
          "TYPE": "Disrupted",
          "reason": {
            "TYPE": "Other",
            "problem": {
              "message": "PROBLEM"
            }
          }
        },
        "lockPaths": []
      }""")
  }

  "OrderFailedInFork" in {
    check(OrderFailedInFork(Position(1), Some(Outcome.Failed(NamedValues.rc(1)))), json"""
      {
        "TYPE": "OrderFailedInFork",
        "movedTo": [ 1 ],
        "outcome": {
          "TYPE": "Failed",
          "namedValues": {
            "returnCode": 1
          }
        },
        "lockPaths": []
      }""")
  }

  "OrderFailedInFork complete" in {
    check(OrderFailedInFork(Position(1), Some(Outcome.Failed(Some("ERROR"), NamedValues.rc(1))), Seq(LockPath("LOCK"))), json"""
      {
        "TYPE": "OrderFailedInFork",
        "movedTo": [ 1 ],
        "outcome": {
          "TYPE": "Failed",
          "namedValues": {
            "returnCode": 1
          },
          "message": "ERROR"
        },
        "lockPaths": [ "LOCK" ]
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
          "TYPE": "Succeeded"
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
    check(OrderCancelMarked(CancelMode.FreshOnly), json"""
      {
        "TYPE": "OrderCancelMarked",
        "mode": {
          "TYPE": "FreshOnly"
        }
      }""")
  }

  "OrderCancelled" in {
    check(OrderCancelled, json"""
      {
        "TYPE": "OrderCancelled"
      }""")
  }

  "OrderRemoveMarked" in {
    check(OrderRemoveMarked, json"""
      {
        "TYPE": "OrderRemoveMarked"
      }""")
  }

  "OrderRemoved" in {
    check(OrderRemoved, json"""
      {
        "TYPE": "OrderRemoved"
      }""")
  }

  "OrderSuspendMarked" in {
    check(OrderSuspendMarked(), json"""
      {
        "TYPE": "OrderSuspendMarked",
        "mode": {}
      }""")
  }

  "OrderSuspended" in {
    check(OrderSuspended, json"""
      {
        "TYPE": "OrderSuspended"
      }""")
  }

  "OrderResumeMarked" in {
    check(OrderResumeMarked(Some(Position(1)), Some(Seq(HistoricOutcome(Position(0), Outcome.succeeded)))), json"""
      {
        "TYPE": "OrderResumeMarked",
        "position": [ 1 ],
        "historicOutcomes": [
          {
            "position": [ 0 ],
            "outcome":{
              "TYPE":  "Succeeded"
            }
          }
        ]
      }""")
  }

  "OrderResumed" in {
    check(OrderResumed(Some(Position(1)), Some(Seq(HistoricOutcome(Position(0), Outcome.succeeded)))), json"""
      {
        "TYPE": "OrderResumed",
        "position": [ 1 ],
        "historicOutcomes": [
          {
            "position": [ 0 ],
            "outcome":{
              "TYPE":  "Succeeded"
            }
          }
        ]
      }""")
  }

  "OrderLockAcquired" in {
    check(OrderLockAcquired(LockPath("LOCK"), Some(3)), json"""
      {
        "TYPE": "OrderLockAcquired",
        "lockPath": "LOCK",
        "count": 3
      }""")
  }

  "OrderLockQueued" in {
    check(OrderLockQueued(LockPath("LOCK"), Some(1)), json"""
      {
        "TYPE": "OrderLockQueued",
        "lockPath": "LOCK",
        "count": 1
      }""")
  }

  "OrderLockReleased" in {
    check(OrderLockReleased(LockPath("LOCK")), json"""
      {
        "TYPE": "OrderLockReleased",
        "lockPath": "LOCK"
      }""")
  }

  private def check(event: OrderEvent, json: => Json) =
    testJson(event, json)

  if (sys.props contains "test.speed") "Speed" in {
    val n = 10000
    val event = Stamped(12345678L, Timestamp.ofEpochMilli(1),
      KeyedEvent[OrderEvent](OrderId("ORDER"), OrderAdded(WorkflowPath("WORKFLOW") ~ "VERSION",
        arguments = Map("KEY" -> StringValue("VALUE")))))
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
