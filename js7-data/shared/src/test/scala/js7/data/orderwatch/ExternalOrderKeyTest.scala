package js7.data.orderwatch

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.Test
import js7.tester.CirceJsonTester.testJson

final class ExternalOrderKeyTest extends Test
{
  "JSON" in {
    testJson(
      ExternalOrderKey(OrderWatchPath("WATCH"), ExternalOrderName("NAME")),
      json"""{
        "orderWatchPath": "WATCH",
        "name": "NAME"
      }""")
  }
}
