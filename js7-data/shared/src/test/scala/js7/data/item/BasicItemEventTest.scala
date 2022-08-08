package js7.data.item

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.circeutils.typed.TypedJsonCodec
import js7.data.agent.AgentPath
import js7.data.controller.ControllerState
import js7.data.item.BasicItemEvent.{ItemAttachable, ItemAttached, ItemAttachedToMe, ItemDeleted, ItemDeletionMarked, ItemDetachable, ItemDetached, ItemDetachingFromMe}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.orderwatch.OrderWatchPath
import js7.data.subagent.SubagentId
import js7.data.workflow.WorkflowPath
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}
import org.scalatest.freespec.AnyFreeSpec

final class BasicItemEventTest extends AnyFreeSpec
{
  implicit private val jsonCodec: TypedJsonCodec[BasicItemEvent] =
    BasicItemEvent.jsonCodec(ControllerState)

  "JSON" - {
    "ItemDeletionMarked" in {
      testJson[BasicItemEvent](ItemDeletionMarked(OrderWatchPath("PATH")), json"""
        {
          "TYPE": "ItemDeletionMarked",
          "key": "OrderWatch:PATH"
        }""")
    }

    "ItemDeleted" in {
      testJson[BasicItemEvent](ItemDeleted(OrderWatchPath("PATH")), json"""
        {
          "TYPE": "ItemDeleted",
          "key": "OrderWatch:PATH"
        }""")
    }

    "ItemAttachable" in {
      val event = ItemAttachable(OrderWatchPath("PATH"), AgentPath("AGENT"))
      testJson[BasicItemEvent](event,
        json"""
        {
          "TYPE": "ItemAttachable",
          "key": "OrderWatch:PATH",
          "delegateId": "Agent:AGENT"
        }""")

      testJsonDecoder[BasicItemEvent](event,
        json"""
        {
          "TYPE": "ItemAttachable",
          "key": "OrderWatch:PATH",
          "agentPath": "AGENT"
        }""")
    }

    "ItemAttached" in {
      val event = ItemAttached(OrderWatchPath("PATH"), Some(ItemRevision(7)), AgentPath("AGENT"))
      testJson[BasicItemEvent](event,
        json"""
        {
          "TYPE": "ItemAttached",
          "key": "OrderWatch:PATH",
          "itemRevision": 7,
          "delegateId": "Agent:AGENT"
        }""")

      testJsonDecoder[BasicItemEvent](event,
        json"""
        {
          "TYPE": "ItemAttached",
          "key": "OrderWatch:PATH",
          "itemRevision": 7,
          "agentPath": "AGENT"
        }""")
    }

    "ItemAttachedToMe" in {
      testJson[BasicItemEvent](
        ItemAttachedToMe(
          JobResource(JobResourcePath("JOB-RESOURCE"), itemRevision = Some(ItemRevision(7)))),
        json"""
        {
          "TYPE": "ItemAttachedToMe",
          "item": {
            "TYPE": "JobResource",
            "path": "JOB-RESOURCE",
            "variables": {},
            "env": {},
            "itemRevision": 7
           }
        }""")

      testJsonDecoder[BasicItemEvent](
        ItemAttachedToMe(
          JobResource(JobResourcePath("JOB-RESOURCE"), itemRevision = Some(ItemRevision(7)))),
        json"""
        {
          "TYPE": "ItemAttachedToAgent",
          "item": {
            "TYPE": "JobResource",
            "path": "JOB-RESOURCE",
            "variables": {},
            "env": {},
            "itemRevision": 7
           }
        }""")
    }

    "ItemDetachable" in {
      val event = ItemDetachable(WorkflowPath("PATH") ~ "1", AgentPath("AGENT"))
      testJson[BasicItemEvent](event,
        json"""
        {
          "TYPE": "ItemDetachable",
          "key": "Workflow:PATH~1",
          "delegateId": "Agent:AGENT"
        }""")

      testJsonDecoder[BasicItemEvent](event,
        json"""
        {
          "TYPE": "ItemDetachable",
          "key": "Workflow:PATH~1",
          "agentPath": "AGENT"
        }""")
    }

    "ItemDetached" in {
      val event = ItemDetached(WorkflowPath("PATH") ~ "1", AgentPath("AGENT"))
      testJson[BasicItemEvent](event,
        json"""
        {
          "TYPE": "ItemDetached",
          "key": "Workflow:PATH~1",
          "delegateId": "Agent:AGENT"
        }""")

      testJsonDecoder[BasicItemEvent](event,
        json"""
        {
          "TYPE": "ItemDetached",
          "key": "Workflow:PATH~1",
          "agentPath": "AGENT"
        }""")
    }

    "ItemDetachingFromMe" in {
      val event = ItemDetachingFromMe(SubagentId("SUBAGENT"))
      testJson[BasicItemEvent](event,
        json"""
        {
          "TYPE": "ItemDetachingFromMe",
          "key": "Subagent:SUBAGENT"
        }""")
    }
  }
}
