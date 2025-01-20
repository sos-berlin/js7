package js7.data.board

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class BoardNoticeKeyTest extends OurTestSuite:

  "JSON" in:
    testJson[BoardNoticeKey](
      BoardPath("BOARD") / NoticeKey("NOTICE"),
      json"""[
        "BOARD", "NOTICE"
      ]""")

    testJsonDecoder[BoardNoticeKey](
      BoardPath("BOARD") / NoticeKey.empty,
      json"""[
        "BOARD"
      ]""")
