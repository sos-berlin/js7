package js7.data.board

import io.circe.Codec
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.test.OurTestSuite
import js7.data.plan.{PlanId, PlanSchemaId}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class NoticePlaceTest extends OurTestSuite:

  "JSON" - {
    "NoticePlace.Snapshot" in:
      given Codec.AsObject[NoticePlace.Snapshot] = TypedJsonCodec(NoticePlace.Snapshot.subtype)

      testJson(
        NoticePlace.Snapshot(
          noticeId = PlanId.Global / BoardPath("BOARD") / "NOTICE",
          isAnnounced = true,
          isInConsumption = true,
          consumptionCount = 3),
        json"""{
          "TYPE": "NoticePlace",
          "noticeId": [ "BOARD", "NOTICE" ],
          "isAnnounced": true,
          "isInConsumption": true,
          "consumptionCount": 3
        }""")

      testJsonDecoder(
        NoticePlace.Snapshot(
          noticeId = PlanSchemaId("DailyPlan") / "2025-01-17" / BoardPath("BOARD") / "NOTICE"),
        json"""{
          "TYPE": "NoticePlace",
          "noticeId": [ "DailyPlan", "2025-01-17", "BOARD", "NOTICE" ]
        }""")

      // COMPATIBLE with v2.7.3
      testJsonDecoder(
        NoticePlace.Snapshot(
          noticeId = PlanId.Global / BoardPath("BOARD") / "NOTICE",
          isAnnounced = true,
          isInConsumption = true,
          consumptionCount = 3),
        json"""{
          "TYPE": "NoticePlace",
          "noticeId": "NOTICE",
          "boardPath": "BOARD",
          "isAnnounced": true,
          "isInConsumption": true,
          "consumptionCount": 3
        }""")

      // COMPATIBLE with v2.7.3
      testJsonDecoder(
        NoticePlace.Snapshot(
          noticeId = PlanId.Global / BoardPath("BOARD") / "NOTICE"),
        json"""{
          "TYPE": "NoticePlace",
          "noticeId": "NOTICE",
          "boardPath": "BOARD"
        }""")
  }
