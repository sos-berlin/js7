package js7.data.board

import cats.syntax.option.*
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.time.Timestamp
import js7.base.time.TimestampForTests.ts
import js7.data.board.NoticeEvent.{NoticeDeleted, NoticeMoved, NoticePosted}
import js7.data.plan.PlanSchemaId
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class NoticeEventTest extends OurTestSuite:

  "NoticePosted" in:
    testJson[NoticeEvent](
      NoticePosted(
        PlanSchemaId("DailyPlan") / "2025-01-17" / NoticeKey("NOTICE"),
        ts"1970-01-01T01:00:00Z".some),
      json"""{
         "TYPE": "NoticePosted",
         "plannedNoticeKey": [ "DailyPlan", "2025-01-17", "NOTICE" ],
         "endOfLife": 3600000
        }""")

    testJson[NoticeEvent](
      NoticePosted(
        GlobalNoticeKey("NOTICE"),
        endOfLife = ts"1970-01-01T01:00:00Z".some),
      json"""{
         "TYPE": "NoticePosted",
         "plannedNoticeKey": "NOTICE",
         "endOfLife": 3600000
        }""")

    testJsonDecoder[NoticeEvent](
      NoticePosted(
        GlobalNoticeKey("NOTICE"),
        endOfLife = ts"1970-01-01T01:00:00Z".some),
      json"""{
         "TYPE": "NoticePosted",
         "notice": {
           "id": "NOTICE",
           "endOfLife": 3600000
         }
        }""")

  "NoticeDeleted" in:
    testJson[NoticeEvent](
      NoticeDeleted(GlobalNoticeKey("NOTICE")),
      json"""{
       "TYPE": "NoticeDeleted",
       "plannedNoticeKey": "NOTICE"
      }""")

    testJsonDecoder[NoticeEvent](
      NoticeDeleted(GlobalNoticeKey("NOTICE")),
      json"""{
       "TYPE": "NoticeDeleted",
       "noticeId": "NOTICE"
      }""")

  "NoticeMoved" in:
    testJson[NoticeEvent](
      NoticeMoved(
        GlobalNoticeKey("2025-01-22/NOTICE"),
        PlanSchemaId("DailyPlan") / "2025-01-22" / NoticeKey("NOTICE"),
        Some(ts"1970-01-01T01:00:00Z")),
      json"""{
       "TYPE": "NoticeMoved",
       "fromPlannedNoticeKey": "2025-01-22/NOTICE",
       "toPlannedNoticeKey": [ "DailyPlan", "2025-01-22", "NOTICE"],
       "endOfLife": 3600000
      }""")
