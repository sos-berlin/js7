package js7.data.ordersource

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.ordersource.OrderSourceEvent.{OrderSourceOrderArised, OrderSourceOrderVanished}
import js7.data.value.{NamedValues, StringValue}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class OrderSourceEventTest extends AnyFreeSpec
{
  "JSON" - {
    "OrderSourceOrderArised" in {
      testJson[OrderSourceEvent](
        OrderSourceOrderArised(SourceOrderName("NAME"), NamedValues("file" -> StringValue("FILE"))),
        json"""{
          "TYPE": "OrderSourceOrderArised",
          "sourceOrderName": "NAME",
          "arguments": {
            "file": "FILE"
          }
        }""")
    }

    "OrderSourceOrderVanished" in {
      testJson[OrderSourceEvent](
        OrderSourceOrderVanished(SourceOrderName("NAME")),
        json"""{
          "TYPE": "OrderSourceOrderVanished",
          "sourceOrderName": "NAME"
        }""")
    }
  }
}
