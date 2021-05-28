package js7.data.orderwatch

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.time.ScalaTime._
import js7.base.utils.SimplePattern
import js7.data.agent.AgentPath
import js7.data.controller.ControllerState
import js7.data.item.ItemAttachedState.Attached
import js7.data.item.{ItemRevision, UnsignedSimpleItemEvent}
import js7.data.order.OrderId
import js7.data.orderwatch.OrderWatchState.{Arised, ExternalOrderSnapshot, HasOrder, Vanished, VanishedAck}
import js7.data.value.expression.Expression.NamedValue
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.WorkflowPath
import js7.tester.CirceJsonTester.testJson
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AsyncFreeSpec

final class OrderWatchStateTest extends AsyncFreeSpec
{
  private val orderWatchState = OrderWatchState(
    FileWatch(
      OrderWatchPath("FILE-WATCH"),
      WorkflowPath("WORKFLOW"),
      AgentPath("AGENT"),
      "DIRECTORY",
      Some(SimplePattern("PATTERN.*\\.csv".r.pattern.pattern)),
      Some(NamedValue("1")),
      delay = 2.s,
      Some(ItemRevision(7))),
    Map(AgentPath("AGENT") -> Attached(Some(ItemRevision(7)))),
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
          OrderWatchPath("FILE-WATCH"),
          ExternalOrderName("FILE"),
          Arised(OrderId("ORDER"), NamedValues("file" -> StringValue("FILE")))),
        json"""{
          "TYPE": "ExternalOrder",
          "externalOrderName": "FILE",
          "orderWatchPath": "FILE-WATCH",
          "state": {
            "TYPE": "Arised",
            "orderId": "ORDER",
            "arguments": { "file": "FILE" }
          }
        }""")

      testJson[OrderWatchState.Snapshot](
        ExternalOrderSnapshot(
          OrderWatchPath("FILE-WATCH"),
          ExternalOrderName("FILE"),
          HasOrder(OrderId("ORDER"), None)),
        json"""{
          "TYPE": "ExternalOrder",
          "externalOrderName": "FILE",
          "orderWatchPath": "FILE-WATCH",
          "state": {
            "TYPE": "HasOrder",
            "orderId": "ORDER"
          }
        }""")

      testJson[OrderWatchState.Snapshot](
        ExternalOrderSnapshot(
          OrderWatchPath("FILE-WATCH"),
          ExternalOrderName("FILE"),
          HasOrder(OrderId("ORDER"), Some(Vanished))),
        json"""{
          "TYPE": "ExternalOrder",
          "externalOrderName": "FILE",
          "orderWatchPath": "FILE-WATCH",
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
          OrderWatchPath("FILE-WATCH"),
          ExternalOrderName("FILE"),
          HasOrder(OrderId("ORDER"), Some(VanishedAck))),
        json"""{
          "TYPE": "ExternalOrder",
          "externalOrderName": "FILE",
          "orderWatchPath": "FILE-WATCH",
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
          OrderWatchPath("FILE-WATCH"),
          ExternalOrderName("FILE"),
          HasOrder(
            OrderId("ORDER"),
            Some(Arised(
              OrderId("NEXT"),
              NamedValues("file" -> StringValue("FILE")))))),
        json"""{
          "TYPE": "ExternalOrder",
          "externalOrderName": "FILE",
          "orderWatchPath": "FILE-WATCH",
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
      implicit val snapshotObjectJsonCodec: TypedJsonCodec[Any] =
        TypedJsonCodec(
          Subtype(UnsignedSimpleItemEvent.jsonCodec(ControllerState)),
          Subtype[OrderWatchState.Snapshot])

      for (snapshot <- orderWatchState.toSnapshot.toListL.runToFuture) yield
        assert(snapshot.asJson ==json"""[
          {
            "TYPE": "UnsignedSimpleItemAdded",
            "item": {
              "TYPE": "FileWatch",
              "path": "FILE-WATCH",
              "workflowPath": "WORKFLOW",
              "agentPath": "AGENT",
              "directory": "DIRECTORY",
              "pattern": "PATTERN.*\\.csv",
              "orderIdExpression": "$$1",
              "delay": 2,
              "itemRevision": 7
            }
          }, {
            "TYPE": "ExternalOrder",
            "orderWatchPath": "FILE-WATCH",
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
            "orderWatchPath": "FILE-WATCH",
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
