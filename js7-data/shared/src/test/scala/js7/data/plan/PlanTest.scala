package js7.data.plan

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.tester.CirceJsonTester.testJson

final class PlanTest extends OurTestSuite:

  "JSON" - {
    "Status" in:
      testJson[Plan.Status](Plan.Status.Open, json""" "Open" """)
      testJson[Plan.Status](Plan.Status.Closed, json""" "Closed" """)
      testJson[Plan.Status](Plan.Status.Finished, json""" "Finished" """)
      testJson[Plan.Status](Plan.Status.Deleted, json""" "Deleted" """)
  }
