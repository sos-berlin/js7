package js7.data.ordersource

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.data.agent.AgentId
import js7.data.agent.AttachedState.Attached
import js7.data.item.ItemRevision
import js7.data.order.OrderId
import js7.data.ordersource.OrderSourceState.{Arised, HasOrder, Vanished}
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.WorkflowPath
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class OrderSourceStateTest extends AnyFreeSpec
{
  private val orderSourceState = OrderSourceState(
    FileOrderSource(
      OrderSourceId("SOURCE"),
      WorkflowPath("WORKFLOW"),
      AgentId("AGENT"),
    "DIRECTORY",
    Some("PATTERN.*\\.csv".r.pattern),
    ItemRevision(7)),
    Some(Attached),
    Map( // Not in snapshot, because its duplicate to Order.orderSourceKey
      SourceOrderName("A-NAME") -> Arised(NamedValues("K" -> StringValue("V"))),
      SourceOrderName("B-NAME") -> HasOrder(OrderId("B-ORDER"), Some(Vanished))))

  "recoverQueues" in {
    assert(orderSourceState.arisedQueue == Set(SourceOrderName("A-NAME")))
    assert(orderSourceState.vanishedQueue == Set(SourceOrderName("B-NAME")))
  }

  "toSnapshot JSON" in {
    assert(orderSourceState.toSnapshot.toListL.await(99.s).asJson ==json"""[
      {
        "TYPE": "OrderSourceState.Header",
        "orderSource": {
          "TYPE": "FileOrderSource",
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
        "TYPE": "OrderSourceState.SourceOrder",
        "orderSourceId": "SOURCE",
        "sourceOrderName": "A-NAME",
        "state": {
          "TYPE": "Arised",
          "arguments": {
            "K": "V"
          }
        }
      }, {
        "TYPE": "OrderSourceState.SourceOrder",
        "orderSourceId": "SOURCE",
        "sourceOrderName": "B-NAME",
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
