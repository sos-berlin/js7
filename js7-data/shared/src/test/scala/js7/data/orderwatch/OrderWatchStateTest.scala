package js7.data.orderwatch

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.utils.SimplePattern
import js7.data.agent.AgentId
import js7.data.agent.AttachedState.Attached
import js7.data.item.ItemRevision
import js7.data.order.OrderId
import js7.data.orderwatch.OrderWatchState.{Arised, ExternalOrderSnapshot, HasOrder, Vanished, VanishedAck}
import js7.data.value.expression.Expression.NamedValue
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.WorkflowPath
import js7.tester.CirceJsonTester.testJson
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class OrderWatchStateTest extends AnyFreeSpec
{
  private val orderWatchState = OrderWatchState(
    FileWatch(
      OrderWatchId("FILE-WATCH"),
      WorkflowPath("WORKFLOW"),
      AgentId("AGENT"),
      "DIRECTORY",
      Some(SimplePattern("PATTERN.*\\.csv".r.pattern.pattern)),
      Some(NamedValue("1")),
      delay = 2.s,
      ItemRevision(7)),
    Some(Attached),
    Map( // Not in snapshot, because its duplicate to Order.externalOrderKey
      ExternalOrderName("A-NAME") -> Arised(OrderId("A-ORDER"), NamedValues("K" -> StringValue("V"))),
      ExternalOrderName("B-NAME") -> HasOrder(OrderId("B-ORDER"), Some(Vanished))))

  "recoverQueues" in {
    assert(orderWatchState.arisedQueue == Set(ExternalOrderName("A-NAME")))
    assert(orderWatchState.vanishedQueue == Set(ExternalOrderName("B-NAME")))
  }

  "JSON" - {
    "ExternalOrderSnapshot" in {
      testJson[OrderWatchState.Snapshot](
        ExternalOrderSnapshot(
          OrderWatchId("FILE-WATCH"),
          ExternalOrderName("FILE"),
          Arised(OrderId("ORDER"), NamedValues("file" -> StringValue("FILE")))),
        json"""{
          "TYPE": "ExternalOrder",
          "externalOrderName": "FILE",
          "orderWatchId": "FILE-WATCH",
          "state": {
            "TYPE": "Arised",
            "orderId": "ORDER",
            "arguments": { "file": "FILE" }
          }
        }""")

      testJson[OrderWatchState.Snapshot](
        ExternalOrderSnapshot(
          OrderWatchId("FILE-WATCH"),
          ExternalOrderName("FILE"),
          HasOrder(OrderId("ORDER"), None)),
        json"""{
          "TYPE": "ExternalOrder",
          "externalOrderName": "FILE",
          "orderWatchId": "FILE-WATCH",
          "state": {
            "TYPE": "HasOrder",
            "orderId": "ORDER"
          }
        }""")

      testJson[OrderWatchState.Snapshot](
        ExternalOrderSnapshot(
          OrderWatchId("FILE-WATCH"),
          ExternalOrderName("FILE"),
          HasOrder(OrderId("ORDER"), Some(Vanished))),
        json"""{
          "TYPE": "ExternalOrder",
          "externalOrderName": "FILE",
          "orderWatchId": "FILE-WATCH",
          "state": {
            "TYPE": "HasOrder",
            "orderId": "ORDER",
            "queued": {
              "TYPE": "Vanished"
            }
          }
        }""")

      testJson[OrderWatchState.Snapshot](
        ExternalOrderSnapshot(
          OrderWatchId("FILE-WATCH"),
          ExternalOrderName("FILE"),
          HasOrder(OrderId("ORDER"), Some(VanishedAck))),
        json"""{
          "TYPE": "ExternalOrder",
          "externalOrderName": "FILE",
          "orderWatchId": "FILE-WATCH",
          "state": {
            "TYPE": "HasOrder",
            "orderId": "ORDER",
            "queued": {
              "TYPE": "VanishedAck"
            }
          }
        }""")

      testJson[OrderWatchState.Snapshot](
        ExternalOrderSnapshot(
          OrderWatchId("FILE-WATCH"),
          ExternalOrderName("FILE"),
          HasOrder(
            OrderId("ORDER"),
            Some(Arised(
              OrderId("NEXT"),
              NamedValues("file" -> StringValue("FILE")))))),
        json"""{
          "TYPE": "ExternalOrder",
          "externalOrderName": "FILE",
          "orderWatchId": "FILE-WATCH",
          "state": {
            "TYPE": "HasOrder",
            "orderId": "ORDER",
            "queued": {
              "TYPE": "Arised",
              "orderId": "NEXT",
              "arguments": { "file": "FILE" }
            }
          }
        }""")
    }

    "toSnapshot" in {
      assert(orderWatchState.toSnapshot.toListL.await(99.s).asJson ==json"""[
        {
          "TYPE": "OrderWatchState.Header",
          "orderWatch": {
            "TYPE": "FileWatch",
            "id": "FILE-WATCH",
            "workflowPath": "WORKFLOW",
            "agentId": "AGENT",
            "directory": "DIRECTORY",
            "pattern": "PATTERN.*\\.csv",
            "orderIdExpression": "$$1",
            "delay": 2,
            "itemRevision": 7
          },
          "attached": {
            "TYPE": "Attached"
          }
        }, {
          "TYPE": "ExternalOrder",
          "orderWatchId": "FILE-WATCH",
          "externalOrderName": "A-NAME",
          "state": {
            "TYPE": "Arised",
            "orderId": "A-ORDER",
            "arguments": {
              "K": "V"
            }
          }
        }, {
          "TYPE": "ExternalOrder",
          "orderWatchId": "FILE-WATCH",
          "externalOrderName": "B-NAME",
          "state": {
            "TYPE": "HasOrder",
            "orderId": "B-ORDER",
            "queued": {
              "TYPE": "Vanished"
            }
          }
        }
      ]""")
    }
  }
}
