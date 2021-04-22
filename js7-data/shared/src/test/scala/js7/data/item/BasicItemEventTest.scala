package js7.data.item

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.agent.AgentId
import js7.data.controller.ControllerState
import js7.data.item.BasicItemEvent.{ItemAttachable, ItemAttached, ItemAttachedToAgent, ItemDeletionMarked, ItemDestroyed, ItemDetachable, ItemDetached}
import js7.data.job.{JobResource, JobResourceId}
import js7.data.orderwatch.OrderWatchId
import js7.data.workflow.WorkflowPath
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class BasicItemEventTest extends AnyFreeSpec
{
  implicit private val jsonCodec = BasicItemEvent.jsonCodec(ControllerState)

  "JSON" - {
    "ItemDeletionMarked" in {
      testJson[BasicItemEvent](ItemDeletionMarked(OrderWatchId("ID")), json"""
        {
          "TYPE": "ItemDeletionMarked",
          "id": "OrderWatch:ID"
        }""")
    }

    "ItemDestroyed" in {
      testJson[BasicItemEvent](ItemDestroyed(OrderWatchId("ID")), json"""
        {
          "TYPE": "ItemDestroyed",
          "id": "OrderWatch:ID"
        }""")
    }

    "ItemAttachable" in {
      testJson[BasicItemEvent](ItemAttachable(OrderWatchId("ID"), AgentId("AGENT")),
        json"""
        {
          "TYPE": "ItemAttachable",
          "id": "OrderWatch:ID",
          "agentId": "AGENT"
        }""")
    }

    "ItemAttached" in {
      testJson[BasicItemEvent](ItemAttached(OrderWatchId("ID"), Some(ItemRevision(7)), AgentId("AGENT")),
        json"""
        {
          "TYPE": "ItemAttached",
          "id": "OrderWatch:ID",
          "itemRevision":  7,
          "agentId": "AGENT"
        }""")
    }

    "ItemAttachedToAgent" in {
      testJson[BasicItemEvent](ItemAttachedToAgent(JobResource(JobResourceId("JOB-RESOURCE"), itemRevision = Some(ItemRevision(7)))),
        json"""
        {
          "TYPE": "ItemAttachedToAgent",
          "item": {
            "TYPE": "JobResource",
            "id": "JOB-RESOURCE",
            "env": {},
            "itemRevision": 7
           }
        }""")
    }

    "ItemDetachable" in {
      testJson[BasicItemEvent](ItemDetachable(WorkflowPath("PATH") ~ "1", AgentId("AGENT")),
        json"""
        {
          "TYPE": "ItemDetachable",
          "id": "Workflow:PATH~1",
          "agentId": "AGENT"
        }""")
    }

    "ItemDetached" in {
      testJson[BasicItemEvent](ItemDetached(WorkflowPath("PATH") ~ "1", AgentId("AGENT")),
        json"""
        {
          "TYPE": "ItemDetached",
          "id": "Workflow:PATH~1",
          "agentId": "AGENT"
        }""")
    }
  }
}
