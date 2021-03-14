package js7.data.orderwatch

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.data.agent.AgentId
import js7.data.agent.AttachedState.Attached
import js7.data.item.ItemRevision
import js7.data.order.OrderId
import js7.data.orderwatch.OrderWatchState.{Arised, ExternalOrderSnapshot, HasOrder, Vanished, VanishedAck}
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.WorkflowPath
import js7.tester.CirceJsonTester.testJson
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class OrderWatchStateTest extends AnyFreeSpec
{
  private val orderWatchState = OrderWatchState(
    FileWatch(
      OrderWatchId("SOURCE"),
      WorkflowPath("WORKFLOW"),
      AgentId("AGENT"),
    "DIRECTORY",
    Some("PATTERN.*\\.csv".r.pattern),
    ItemRevision(7)),
    Some(Attached),
    Map( // Not in snapshot, because its duplicate to Order.orderSourceKey
      ExternalOrderName("A-NAME") -> Arised(NamedValues("K" -> StringValue("V"))),
      ExternalOrderName("B-NAME") -> HasOrder(OrderId("B-ORDER"), Some(Vanished))))

  "recoverQueues" in {
    assert(orderWatchState.arisedQueue == Set(ExternalOrderName("A-NAME")))
    assert(orderWatchState.vanishedQueue == Set(ExternalOrderName("B-NAME")))
  }

  "JSON" - {
    "ExternalOrderSnapshot" in {
      testJson[OrderWatchState.Snapshot](
        ExternalOrderSnapshot(
          OrderWatchId("WATCH"),
          ExternalOrderName("FILE"),
          Arised(NamedValues("file" -> StringValue("FILE")))),
        json"""{
          "TYPE": "ExternalOrder",
          "externalOrderName": "FILE",
          "orderWatchId": "WATCH",
          "state": {
            "TYPE": "Arised",
            "arguments": { "file": "FILE" }
          }
        }""")

      testJson[OrderWatchState.Snapshot](
        ExternalOrderSnapshot(
          OrderWatchId("WATCH"),
          ExternalOrderName("FILE"),
          HasOrder(OrderId("ORDER"), None)),
        json"""{
          "TYPE": "ExternalOrder",
          "externalOrderName": "FILE",
          "orderWatchId": "WATCH",
          "state": {
            "TYPE": "HasOrder",
            "orderId": "ORDER"
          }
        }""")

      testJson[OrderWatchState.Snapshot](
        ExternalOrderSnapshot(
          OrderWatchId("WATCH"),
          ExternalOrderName("FILE"),
          HasOrder(OrderId("ORDER"), Some(Vanished))),
        json"""{
          "TYPE": "ExternalOrder",
          "externalOrderName": "FILE",
          "orderWatchId": "WATCH",
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
          OrderWatchId("WATCH"),
          ExternalOrderName("FILE"),
          HasOrder(OrderId("ORDER"), Some(VanishedAck))),
        json"""{
          "TYPE": "ExternalOrder",
          "externalOrderName": "FILE",
          "orderWatchId": "WATCH",
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
          OrderWatchId("WATCH"),
          ExternalOrderName("FILE"),
          HasOrder(OrderId("ORDER"), Some(Arised(NamedValues("file" -> StringValue("FILE")))))),
        json"""{
          "TYPE": "ExternalOrder",
          "externalOrderName": "FILE",
          "orderWatchId": "WATCH",
          "state": {
            "TYPE": "HasOrder",
            "orderId": "ORDER",
            "queued": {
              "TYPE": "Arised",
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
            "id": "SOURCE",
            "workflowPath": "WORKFLOW",
            "agentId": "AGENT",
            "directory": "DIRECTORY",
            "pattern": "PATTERN.*\\.csv",
            "itemRevision": 7
          },
          "attached": {
            "TYPE": "Attached"
          }
        }, {
          "TYPE": "ExternalOrder",
          "orderWatchId": "SOURCE",
          "externalOrderName": "A-NAME",
          "state": {
            "TYPE": "Arised",
            "arguments": {
              "K": "V"
            }
          }
        }, {
          "TYPE": "ExternalOrder",
          "orderWatchId": "SOURCE",
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
