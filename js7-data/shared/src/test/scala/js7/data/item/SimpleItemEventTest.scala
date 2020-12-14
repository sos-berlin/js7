package js7.data.item

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.item.SimpleItemEvent.{SimpleItemAdded, SimpleItemChanged}
import js7.data.lock.{Lock, LockId}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class SimpleItemEventTest extends AnyFreeSpec
{
  implicit private val jsonCodec = SimpleItemEvent.jsonCodec(Seq(Lock))

  "JSON" in {
    testJson[SimpleItemEvent](SimpleItemAdded(Lock(LockId("LOCK"), limit = 1)), json"""
      {
        "TYPE": "SimpleItemAdded",
        "item": {
          "TYPE": "Lock",
          "id": "LOCK",
          "limit": 1
        }
      }""")

    testJson[SimpleItemEvent](SimpleItemChanged(Lock(LockId("LOCK"), limit = 3)), json"""
      {
        "TYPE": "SimpleItemChanged",
        "item": {
          "TYPE": "Lock",
          "id": "LOCK",
          "limit": 3
        }
      }""")
  }
}
