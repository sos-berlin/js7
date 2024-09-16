package js7.data.orderwatch

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.SimplePattern
import js7.data.agent.AgentPath
import js7.data.controller.ControllerState
import js7.data.item.{ItemRevision, UnsignedSimpleItemEvent}
import js7.data.order.OrderId
import js7.data.orderwatch.OrderWatchState.{Arised, ArisedOrHasOrder, ExternalOrderSnapshot, HasOrder, Vanished}
import js7.data.value.expression.Expression.NamedValue
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.WorkflowPath
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class OrderWatchStateTest extends OurAsyncTestSuite:

  private val orderWatchState =
    OrderWatchState(
      FileWatch(
        OrderWatchPath("FILE-WATCH"),
        WorkflowPath("WORKFLOW"),
        AgentPath("AGENT"),
        expr("'DIRECTORY'"),
        Some(SimplePattern("PATTERN.*\\.csv".r.pattern.pattern)),
        Some(NamedValue("1")),
        delay = 2.s,
        Some(ItemRevision(7))),
      Map( // Not in snapshot, because its duplicate to Order.externalOrder
        ExternalOrderName("A-NAME") -> Arised(OrderId("A-ORDER"), NamedValues("K" -> StringValue("V"))),
        ExternalOrderName("B-NAME") -> HasOrder(OrderId("B-ORDER"), None),
        ExternalOrderName("C-NAME") -> HasOrder(OrderId("C-ORDER"), Some(Vanished)),
        ExternalOrderName("C2-NAME") -> HasOrder(OrderId("C2-ORDER"), Some(Vanished)),
        ExternalOrderName("D-NAME") -> HasOrder(OrderId("D-ORDER"), Some(Arised(OrderId("D-ORDER"), NamedValues("K" -> StringValue("V2")))))),
      orderAddedQueue = Set(ExternalOrderName("B-NAME")),
      orderExternalVanishedQueue = Set(ExternalOrderName("C2-NAME")))

  "recoverQueues" in:
    assert(orderWatchState.orderAddedQueue == Set(ExternalOrderName("B-NAME")))
    assert(orderWatchState.orderExternalVanishedQueue == Set(ExternalOrderName("C2-NAME")))

  "JSON" - {
    "ArisedOrHasOrder" in:
      testJson[ArisedOrHasOrder](Arised(OrderId("ORDER"), Map("file" -> StringValue("FILE"))),
        json"""{
          "TYPE": "Arised",
          "orderId": "ORDER",
          "arguments": {
            "file": "FILE"
          }
        }""")

      testJson[ArisedOrHasOrder](HasOrder(OrderId("ORDER"), Some(Vanished)),
        json"""{
          "TYPE": "HasOrder",
          "orderId": "ORDER",
          "queued": {
            "TYPE": "Vanished"
          }
        }""")

      // Until v2.2.1 exists VanishedAck. It is equivalent to Vanished.
      testJsonDecoder[ArisedOrHasOrder](HasOrder(OrderId("ORDER"), Some(Vanished)),
        json"""{
          "TYPE": "HasOrder",
          "orderId": "ORDER",
          "queued": {
            "TYPE": "VanishedAck"
          }
        }""")

      testJson[ArisedOrHasOrder](
        HasOrder(
          OrderId("ORDER"),
          Some(Arised(OrderId("ORDER"), Map("file" -> StringValue("FILE"))))),
        json"""{
          "TYPE": "HasOrder",
          "orderId": "ORDER",
          "queued": {
            "TYPE": "Arised",
            "orderId": "ORDER",
            "arguments": {
              "file": "FILE"
            }
          }
        }""")

    "ExternalOrderSnapshot" in:
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

    "toSnapshot" in:
      implicit val snapshotObjectJsonCodec: TypedJsonCodec[Any] =
        TypedJsonCodec(
          Subtype(UnsignedSimpleItemEvent.jsonCodec(ControllerState)),
          Subtype[OrderWatchState.Snapshot])

      for snapshot <- orderWatchState.toSnapshotStream.compile.toVector yield
        testJsonDecoder(snapshot, json"""[
          {
            "TYPE": "UnsignedSimpleItemAdded",
            "item": {
              "TYPE": "FileWatch",
              "path": "FILE-WATCH",
              "workflowPath": "WORKFLOW",
              "agentPath": "AGENT",
              "directoryExpr": "'DIRECTORY'",
              "pattern": "PATTERN.*\\.csv",
              "orderIdExpression": "$$1",
              "delay": 2,
              "itemRevision": 7
            }
          }, {
            "TYPE": "ExternalOrder",
            "orderWatchPath": "FILE-WATCH",
            "externalOrderName": "C-NAME",
            "state": {
              "TYPE": "HasOrder",
              "orderId": "C-ORDER",
              "queued": {
                "TYPE": "Vanished"
              }
            }
          }, {
            "TYPE": "ExternalOrder",
            "orderWatchPath": "FILE-WATCH",
            "externalOrderName": "B-NAME",
            "state": {
              "TYPE": "HasOrder",
              "orderId": "B-ORDER"
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
            "externalOrderName": "C2-NAME",
            "state": {
              "TYPE": "HasOrder",
              "orderId": "C2-ORDER",
              "queued": {
                "TYPE": "Vanished"
              }
            }
          }, {
            "TYPE": "ExternalOrder",
            "orderWatchPath": "FILE-WATCH",
            "externalOrderName": "D-NAME",
            "state": {
              "TYPE": "HasOrder",
              "orderId": "D-ORDER",
              "queued": {
                "TYPE": "Arised",
                "orderId": "D-ORDER",
                "arguments": {
                  "K": "V2"
                }
              }
            }
          }
        ]""")
  }
