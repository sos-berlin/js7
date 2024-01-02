package js7.data.orderwatch

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.order.OrderId
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.value.{NamedValues, StringValue}
import js7.tester.CirceJsonTester.testJson

final class OrderWatchEventTest extends OurTestSuite
{
  "JSON" - {
    "ExternalOrderArised" in {
      testJson[OrderWatchEvent](
        ExternalOrderArised(
          ExternalOrderName("NAME"),
          OrderId("ORDER"),
          NamedValues("file" -> StringValue("FILE"))),
        json"""{
          "TYPE": "ExternalOrderArised",
          "externalOrderName": "NAME",
          "orderId": "ORDER",
          "arguments": {
            "file": "FILE"
          }
        }""")
    }

    "ExternalOrderVanished" in {
      testJson[OrderWatchEvent](
        ExternalOrderVanished(ExternalOrderName("NAME")),
        json"""{
          "TYPE": "ExternalOrderVanished",
          "externalOrderName": "NAME"
        }""")
    }
  }
}
