package js7.data.orderwatch

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.data.order.OrderId
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderRejected, ExternalOrderVanished}
import js7.data.value.{NamedValues, StringValue}
import js7.tester.CirceJsonTester.testJson

final class OrderWatchEventTest extends OurTestSuite:

  "JSON" - {
    "ExternalOrderArised" in:
      testJson[OrderWatchEvent](
        ExternalOrderArised(
          ExternalOrderName("NAME"),
          NamedValues("file" -> StringValue("FILE"))),
        json"""{
          "TYPE": "ExternalOrderArised",
          "externalOrderName": "NAME",
          "arguments": {
            "file": "FILE"
          }
        }""")

    "ExternalOrderArised <v2.7.4" in: // COMPATIBLE with v2.7.3
      testJson[OrderWatchEvent](
        ExternalOrderArised(
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
