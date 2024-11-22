package js7.data.board

import io.circe.Codec
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.test.OurTestSuite
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class NoticePlaceTest extends OurTestSuite:

  "JSON" in:
    given Codec.AsObject[NoticePlace.Snapshot] = TypedJsonCodec(NoticePlace.Snapshot.subtype)

    testJson(
      NoticePlace.Snapshot(
        BoardPath("BOARD"),
        NoticeId("NOTICE"),
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

    testJsonDecoder(
      NoticePlace.Snapshot(BoardPath("BOARD"), NoticeId("NOTICE")),
      json"""{
        "TYPE": "NoticePlace",
        "noticeId": "NOTICE",
        "boardPath": "BOARD"
      }""")
