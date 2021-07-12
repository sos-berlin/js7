package js7.data.board

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.board.BoardEvent.NoticeDeleted
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class BoardEventTest extends AnyFreeSpec
{
  "NoticeDeleted" in {
    testJson[BoardEvent](
      NoticeDeleted(NoticeId("NOTICE")),
      json"""{
         "TYPE": "NoticeDeleted",
         "noticeId": "NOTICE"
        }""")
  }
}
