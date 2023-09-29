package js7.data.orderwatch

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.tester.CirceJsonTester.testJson

final class ExternalOrderKeyTest extends OurTestSuite:
  "JSON" in:
    testJson(
      ExternalOrderKey(OrderWatchPath("WATCH"), ExternalOrderName("NAME")),
      json"""{
        "orderWatchPath": "WATCH",
        "name": "NAME"
      }""")
