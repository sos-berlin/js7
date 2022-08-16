package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.board.BoardPath
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.testJsonDecoder

final class PostNoticeTest extends OurTestSuite
{
  "JSON" in {
    // COMPATIBLE with v2.3
    testJsonDecoder[Instruction](
      PostNotices(Seq(BoardPath("BOARD"))),
      json"""
        {
          "TYPE": "PostNotice",
          "boardPath": "BOARD"
        }""")
  }
}
