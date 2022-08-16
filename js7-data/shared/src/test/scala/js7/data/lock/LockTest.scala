package js7.data.lock

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.tester.CirceJsonTester.testJson

final class LockTest extends OurTestSuite
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
