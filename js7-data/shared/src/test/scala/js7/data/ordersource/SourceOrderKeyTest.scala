package js7.data.ordersource

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class SourceOrderKeyTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(
      SourceOrderKey(OrderSourceId("SOURCE"), SourceOrderName("NAME")),
      json"""{
        "orderSourceId": "SOURCE",
        "name": "NAME"
      }""")
  }
}
