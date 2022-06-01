package js7.data.board

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.time.Timestamp
import js7.data.order.OrderId
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class NoticePlaceTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(
      NoticePlace(
        Some(Notice(
          NoticeId("NOTICE"),
          BoardPath("BOARD"),
          Timestamp("1970-01-01T01:00:00Z"))),
        Some(NoticeExpectation(
          NoticeId("NOTICE"),
          Set(OrderId("ORDER"))))),
      json"""
      {
        "notice": {
          "id": "NOTICE",
          "boardPath": "BOARD",
          "endOfLife": 3600000
        },
        "expectation": {
          "id": "NOTICE",
          "orderIds": [ "ORDER" ]
        }
      }""")
  }
}
