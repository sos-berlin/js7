package js7.data.orderwatch

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.value.{NamedValues, StringValue}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class OrderWatchEventTest extends AnyFreeSpec
{
  "JSON" - {
    "ExternalOrderArised" in {
      testJson[OrderWatchEvent](
        ExternalOrderArised(ExternalOrderName("NAME"), NamedValues("file" -> StringValue("FILE"))),
        json"""{
          "TYPE": "ExternalOrderArised",
          "externalOrderName": "NAME",
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
