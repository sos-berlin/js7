package js7.data.item

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.agent.AgentPath
import js7.data.controller.ControllerState
import js7.data.item.BasicItemEvent.{ItemAttachable, ItemAttached, ItemAttachedToAgent, ItemDeletionMarked, ItemDestroyed, ItemDetachable, ItemDetached}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.orderwatch.OrderWatchPath
import js7.data.workflow.WorkflowPath
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class BasicItemEventTest extends AnyFreeSpec
{
  implicit private val jsonCodec = BasicItemEvent.jsonCodec(ControllerState)

  "JSON" - {
    "ItemDeletionMarked" in {
      testJson[BasicItemEvent](ItemDeletionMarked(OrderWatchPath("ID")), json"""
        {
          "TYPE": "ItemDeletionMarked",
          "key": "OrderWatch:ID"
        }""")
    }

    "ItemDestroyed" in {
      testJson[BasicItemEvent](ItemDestroyed(OrderWatchPath("ID")), json"""
        {
          "TYPE": "ItemDestroyed",
          "key": "OrderWatch:ID"
        }""")
    }

    "ItemAttachable" in {
      testJson[BasicItemEvent](ItemAttachable(OrderWatchPath("ID"), AgentPath("AGENT")),
        json"""
        {
          "TYPE": "ItemAttachable",
          "key": "OrderWatch:ID",
          "agentPath": "AGENT"
        }""")
    }

    "ItemAttached" in {
      testJson[BasicItemEvent](ItemAttached(OrderWatchPath("ID"), Some(ItemRevision(7)), AgentPath("AGENT")),
        json"""
        {
          "TYPE": "ItemAttached",
          "key": "OrderWatch:ID",
          "itemRevision":  7,
          "agentPath": "AGENT"
        }""")
    }

    "ItemAttachedToAgent" in {
      testJson[BasicItemEvent](ItemAttachedToAgent(JobResource(JobResourcePath("JOB-RESOURCE"), itemRevision = Some(ItemRevision(7)))),
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
      testJson[BasicItemEvent](ItemDetachable(WorkflowPath("PATH") ~ "1", AgentPath("AGENT")),
        json"""
        {
          "TYPE": "ItemDetachable",
          "key": "Workflow:PATH~1",
          "agentPath": "AGENT"
        }""")
    }

    "ItemDetached" in {
      testJson[BasicItemEvent](ItemDetached(WorkflowPath("PATH") ~ "1", AgentPath("AGENT")),
        json"""
        {
          "TYPE": "ItemDetached",
          "key": "Workflow:PATH~1",
          "agentPath": "AGENT"
        }""")
    }
  }
}
