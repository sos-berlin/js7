package js7.data.board

import cats.syntax.option.*
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.time.Timestamp
import js7.data.board.NoticeEvent.{NoticeDeleted, NoticePosted}
import js7.tester.CirceJsonTester.testJson

final class NoticeEventTest extends OurTestSuite:

  "NoticePosted" in:
    testJson[NoticeEvent](
      NoticePosted(
        NoticePosted.PostedNotice(NoticeId("NOTICE"), Timestamp("1970-01-01T01:00:00Z").some)),
      json"""{
         "TYPE": "NoticePosted",
         "notice": {
           "id": "NOTICE",
           "endOfLife": 3600000
         }
        }""")

  "NoticeDeleted" in:
    testJson[NoticeEvent](
      NoticeDeleted(NoticeId("NOTICE")),
      json"""{
         "TYPE": "NoticeDeleted",
         "noticeId": "NOTICE"
        }""")
