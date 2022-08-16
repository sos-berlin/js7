package js7.data.order

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.base.time.Timestamp
import js7.tester.CirceJsonTester.testJson

final class CycleStateTest extends OurTestSuite
{
  "JSON" in {
    testJson(
      CycleState(
        end = Timestamp("2021-10-01T00:00:00Z"),
        schemeIndex = 1,
        index = 2,
        next = Timestamp("2021-10-01T12:00:00Z")),
      json"""{
        "end": 1633046400000,
        "schemeIndex": 1,
        "index": 2,
        "next": 1633089600000
      }""")
  }
}
