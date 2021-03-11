package js7.data.ordersource

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.thread.MonixBlocking.syntax._
import js7.data.agent.AgentId
import js7.data.agent.AttachedState.{Attached, Attaching}
import js7.data.item.ItemRevision
import js7.data.order.OrderId
import js7.data.workflow.WorkflowPath
import org.scalatest.freespec.AnyFreeSpec
import js7.base.time.ScalaTime._
import monix.execution.Scheduler.Implicits.global

final class OrderSourceStateTest extends AnyFreeSpec
{
  "toSnapshot JSON" in {
    val orderSourceState = OrderSourceState(
      FileOrderSource(
        OrderSourceId("SOURCE"),
        WorkflowPath("WORKFLOW"),
        AgentId("AGENT"),
      "DIRECTORY",
      Some("PATTERN.*\\.csv".r.pattern),
      ItemRevision(7)),
      Some(Attached),
      Map( // Not in snapshot, because its duplicate to Order.orderSourceKey
        SourceOrderName("NAME") -> OrderId("ORDER")))

    assert(orderSourceState.toSnapshot.toListL.await(99.s).asJson ==json"""[
      {
        "TYPE": "OrderSourceState.Snapshot",
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
      }
    ]""")
  }
}
