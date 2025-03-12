package js7.data.plan

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.time.TimestampForTests.ts
import js7.tester.CirceJsonTester.testJson

final class PlanStatusTest extends OurTestSuite:

  "JSON" - {
    "PlanStatus" in:
      testJson[PlanStatus](
        PlanStatus.Open,
        json""" "Open" """)

      testJson[PlanStatus](
        PlanStatus.Closed,
        json""" "Closed" """)

      testJson[PlanStatus](
        PlanStatus.Finished(ts"2025-03-10T12:00:00Z"),
        json"""{
          "TYPE": "Finished",
          "at": 1741608000000
        }""")

      testJson[PlanStatus](
        PlanStatus.Deleted,
        json""" "Deleted" """)
  }
