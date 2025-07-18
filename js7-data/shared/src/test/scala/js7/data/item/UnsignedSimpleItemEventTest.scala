package js7.data.item

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.test.OurTestSuite
import js7.data.controller.ControllerState
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemChanged}
import js7.data.lock.{Lock, LockPath}
import js7.tester.CirceJsonTester.testJson

final class UnsignedSimpleItemEventTest extends OurTestSuite:

  implicit private val jsonCodec: TypedJsonCodec[UnsignedSimpleItemEvent] =
    UnsignedSimpleItemEvent.jsonCodec(using ControllerState)

  "JSON" - {
    "UnsignedSimpleItemAdded" in:
      testJson[UnsignedSimpleItemEvent](UnsignedSimpleItemAdded(Lock(LockPath("LOCK"), limit = 1, Some(ItemRevision(0)))),
        json"""{
          "TYPE": "UnsignedSimpleItemAdded",
          "item": {
            "TYPE": "Lock",
            "path": "LOCK",
            "limit": 1,
            "itemRevision": 0
          }
        }""")

    "UnsignedSimpleItemChanged" in:
      testJson[UnsignedSimpleItemEvent](UnsignedSimpleItemChanged(Lock(LockPath("LOCK"), limit = 3, Some(ItemRevision(1)))),
        json"""{
          "TYPE": "UnsignedSimpleItemChanged",
          "item": {
            "TYPE": "Lock",
            "path": "LOCK",
            "limit": 3,
            "itemRevision": 1
          }
        }""")
  }
