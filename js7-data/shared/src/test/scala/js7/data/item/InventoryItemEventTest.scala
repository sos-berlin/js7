package js7.data.item

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.agent.AgentId
import js7.data.item.CommonItemEvent.{ItemAttachable, ItemAttached, ItemDeletionMarked, ItemDestroyed, ItemDetachable, ItemDetached}
import js7.data.orderwatch.{FileWatch, OrderWatchId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class InventoryItemEventTest extends AnyFreeSpec
{
  implicit private val jsonCodec = InventoryItemEvent.jsonCodec(Seq(FileWatch, Workflow))

  "JSON" - {
    "ItemDeletionMarked" in {
      testJson[InventoryItemEvent](ItemDeletionMarked(OrderWatchId("ID")), json"""
        {
          "TYPE": "ItemDeletionMarked",
          "id": "OrderWatch:ID"
        }""")
    }

    "ItemDestroyed" in {
      testJson[InventoryItemEvent](ItemDestroyed(OrderWatchId("ID")), json"""
        {
          "TYPE": "ItemDestroyed",
          "id": "OrderWatch:ID"
        }""")
    }

    "ItemAttachable" in {
      testJson[InventoryItemEvent](ItemAttachable(OrderWatchId("ID"), AgentId("AGENT")),
        json"""
        {
          "TYPE": "ItemAttachable",
          "id": "OrderWatch:ID",
          "agentId": "AGENT"
        }""")
    }

    "ItemAttached" in {
      testJson[InventoryItemEvent](ItemAttached(OrderWatchId("ID"), Some(ItemRevision(7)), AgentId("AGENT")),
        json"""
        {
          "TYPE": "ItemAttached",
          "id": "OrderWatch:ID",
          "itemRevision":  7,
          "agentId": "AGENT"
        }""")
    }

    "ItemDetachable" in {
      testJson[InventoryItemEvent](ItemDetachable(WorkflowPath("PATH") ~ "1", AgentId("AGENT")),
        json"""
        {
          "TYPE": "ItemDetachable",
          "id": "Workflow:PATH~1",
          "agentId": "AGENT"
        }""")
    }

    "ItemDetached" in {
      testJson[InventoryItemEvent](ItemDetached(WorkflowPath("PATH") ~ "1", AgentId("AGENT")),
        json"""
        {
          "TYPE": "ItemDetached",
          "id": "Workflow:PATH~1",
          "agentId": "AGENT"
        }""")
    }
  }
}
