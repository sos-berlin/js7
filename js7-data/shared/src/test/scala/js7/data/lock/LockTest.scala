package js7.data.lock

import js7.base.circeutils.CirceUtils._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class LockTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(Lock(LockName("LOCK")), json"""
      {
        "name": "LOCK"
      }""")

    testJson(Lock(LockName("LOCK"), nonExclusiveLimit = Some(3)), json"""
      {
        "name": "LOCK",
        "nonExclusiveLimit": 3
      }""")
  }
}
