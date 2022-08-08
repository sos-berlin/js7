package js7.data.lock

import js7.base.circeutils.CirceUtils.*
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class LockTest extends AnyFreeSpec
{
  "JSON" in {
    implicit val x = Lock.jsonCodec
    testJson(Lock(LockPath("LOCK"), limit = 3), json"""
      {
        "path": "LOCK",
        "limit": 3
      }""")
  }

  "LockPath.itemTypeName" in {
    assert(LockPath.itemTypeName == Lock.typeName)
  }
}
