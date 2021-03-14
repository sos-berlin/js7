package js7.data.item

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.agent.AgentId
import js7.data.item.SimpleItemEvent.{SimpleItemAdded, SimpleItemAttachable, SimpleItemAttached, SimpleItemChanged, SimpleItemDeleted}
import js7.data.lock.{Lock, LockId}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class SimpleItemEventTest extends AnyFreeSpec
{
  implicit private val jsonCodec = SimpleItemEvent.jsonCodec(Seq(Lock))

  "JSON" - {
    "SimpleItemAdded" in {
    testJson[SimpleItemEvent](SimpleItemAdded(Lock(LockId("LOCK"), limit = 1)), json"""
      {
        "TYPE": "SimpleItemAdded",
        "item": {
          "TYPE": "Lock",
          "id": "LOCK",
          "limit": 1,
          "itemRevision": 0
        }
      }""")
    }

    "SimpleItemChanged" in {
      testJson[SimpleItemEvent](SimpleItemChanged(Lock(LockId("LOCK"), limit = 3)), json"""
        {
          "TYPE": "SimpleItemChanged",
          "item": {
            "TYPE": "Lock",
            "id": "LOCK",
            "limit": 3,
            "itemRevision": 0
          }
        }""")
    }

    "SimpleItemDeleted" in {
      testJson[SimpleItemEvent](SimpleItemDeleted(LockId("LOCK")), json"""
        {
          "TYPE": "SimpleItemDeleted",
          "id": "Lock:LOCK"
        }""")
    }

    "SimpleItemAttachable" in {
      testJson[SimpleItemEvent](SimpleItemAttachable(LockId("LOCK"), AgentId("AGENT")), json"""
        {
          "TYPE": "SimpleItemAttachable",
          "id": "Lock:LOCK",
          "agentId": "AGENT"
        }""")
    }


    "SimpleItemAttached" in {
      testJson[SimpleItemEvent](SimpleItemAttached(LockId("LOCK"), AgentId("AGENT")), json"""
        {
          "TYPE": "SimpleItemAttached",
          "id": "Lock:LOCK",
          "agentId": "AGENT"
        }""")
    }
  }
}
