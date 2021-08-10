package js7.data.board

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.time.Timestamp
import js7.data.board.BoardEvent.{NoticeDeleted, NoticePosted}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class BoardEventTest extends AnyFreeSpec
{
  "NoticePosted" in {
    testJson[BoardEvent](
      NoticePosted(
        Notice(NoticeId("NOTICE"), Timestamp("1970-01-01T01:00:00Z"))),
      json"""{
         "TYPE": "NoticePosted",
         "notice": {
           "id": "NOTICE",
           "endOfLife": 3600000
         }
        }""")
  }

  "NoticeDeleted" in {
    testJson[BoardEvent](
      NoticeDeleted(NoticeId("NOTICE")),
      json"""{
         "TYPE": "NoticeDeleted",
         "noticeId": "NOTICE"
        }""")
  }
}
