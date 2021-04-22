package js7.data.item

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.controller.ControllerState
import js7.data.item.UnsignedSimpleItemEvent.{SimpleItemAdded, SimpleItemChanged}
import js7.data.lock.{Lock, LockPath}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class UnsignedSimpleItemEventTest extends AnyFreeSpec
{
  implicit private val jsonCodec = UnsignedSimpleItemEvent.jsonCodec(ControllerState)

  "JSON" - {
    "SimpleItemAdded" in {
      testJson[UnsignedSimpleItemEvent](SimpleItemAdded(Lock(LockPath("LOCK"), limit = 1, Some(ItemRevision(0)))),
        json"""{
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
      testJson[UnsignedSimpleItemEvent](SimpleItemChanged(Lock(LockPath("LOCK"), limit = 3, Some(ItemRevision(1)))),
        json"""{
          "TYPE": "SimpleItemChanged",
          "item": {
            "TYPE": "Lock",
            "id": "LOCK",
            "limit": 3,
            "itemRevision": 1
          }
        }""")
    }
  }
}
