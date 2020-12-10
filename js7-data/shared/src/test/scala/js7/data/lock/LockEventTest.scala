package js7.data.lock

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.lock.LockEvent.{LockAdded, LockUpdated}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class LockEventTest extends AnyFreeSpec {

  "JSON" in {
    testJson[LockEvent](LockAdded(limit = 1), json"""
      {
        "TYPE": "LockAdded",
        "limit": 1
      }""")

    testJson[LockEvent](LockUpdated(limit = 3), json"""
      {
        "TYPE": "LockUpdated",
        "limit": 3
      }""")
  }
}
