package js7.data.orderwatch

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class ExternalOrderKeyTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(
      ExternalOrderKey(OrderWatchId("WATCH"), ExternalOrderName("NAME")),
      json"""{
        "orderWatchId": "WATCH",
        "name": "NAME"
      }""")
  }
}
