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
      testJson[BasicItemEvent](ItemDeletionMarked(OrderWatchPath("PATH")), json"""
        {
          "TYPE": "ItemDeletionMarked",
          "key": "OrderWatch:PATH"
        }""")
    }

    "ItemDestroyed" in {
      testJson[BasicItemEvent](ItemDestroyed(OrderWatchPath("PATH")), json"""
        {
          "TYPE": "ItemDestroyed",
          "key": "OrderWatch:PATH"
        }""")
    }

    "ItemAttachable" in {
      testJson[BasicItemEvent](ItemAttachable(OrderWatchPath("PATH"), AgentPath("AGENT")),
        json"""
        {
          "TYPE": "ItemAttachable",
          "key": "OrderWatch:PATH",
          "agentPath": "AGENT"
        }""")
    }

    "ItemAttached" in {
      testJson[BasicItemEvent](ItemAttached(OrderWatchPath("PATH"), Some(ItemRevision(7)), AgentPath("AGENT")),
        json"""
        {
          "TYPE": "ItemAttached",
          "key": "OrderWatch:PATH",
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
            "path": "JOB-RESOURCE",
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
