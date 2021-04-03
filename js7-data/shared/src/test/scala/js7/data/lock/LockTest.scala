package js7.data.lock

import js7.base.circeutils.CirceUtils._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class LockTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(Lock(LockId("LOCK"), limit = 3), json"""
      {
        "id": "LOCK",
        "limit": 3
      }""")
  }
}
