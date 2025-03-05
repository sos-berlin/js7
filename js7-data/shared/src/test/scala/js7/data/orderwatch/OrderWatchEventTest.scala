package js7.data.orderwatch

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.data.order.OrderId
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderAppeared, ExternalOrderRejected, ExternalOrderVanished}
import js7.data.value.{NamedValues, StringValue}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class OrderWatchEventTest extends OurTestSuite:

  "JSON" - {
    "ExternalOrderAppeared" in:
      testJson[OrderWatchEvent](
        ExternalOrderAppeared(
          ExternalOrderName("NAME"),
          NamedValues("file" -> StringValue("FILE"))),
        json"""{
          "TYPE": "ExternalOrderAppeared",
          "externalOrderName": "NAME",
          "arguments": {
            "file": "FILE"
          }
        }""")

    "ExternalOrderArised <v2.7.4" in: // COMPATIBLE with v2.7.3
      testJsonDecoder[OrderWatchEvent](
        ExternalOrderAppeared(
          ExternalOrderName("NAME"),
          NamedValues("file" -> StringValue("FILE")),
          Some(OrderId("ORDER"))),
        json"""{
          "TYPE": "ExternalOrderArised",
          "externalOrderName": "NAME",
          "arguments": {
            "file": "FILE"
          },
          "orderId": "ORDER"
        }""")

    "ExternalOrderRejected" in:
      testJson[OrderWatchEvent](
        ExternalOrderRejected(
          ExternalOrderName("NAME"),
          Problem("PROBLEM")),
        json"""{
          "TYPE": "ExternalOrderRejected",
          "externalOrderName": "NAME",
          "problem": {
            "message": "PROBLEM"
          }
        }""")

    "ExternalOrderVanished" in:
      testJson[OrderWatchEvent](
        ExternalOrderVanished(ExternalOrderName("NAME")),
        json"""{
          "TYPE": "ExternalOrderVanished",
          "externalOrderName": "NAME"
        }""")
  }
