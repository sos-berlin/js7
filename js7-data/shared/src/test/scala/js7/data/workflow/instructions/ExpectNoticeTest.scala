package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.Test
import js7.data.board.{BoardPath, BoardPathExpression}
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.testJsonDecoder

final class ExpectNoticeTest extends Test
{
  "JSON compatible with v2.3" in {
    // COMPATIBLE with v2.3
    testJsonDecoder[Instruction](
      ExpectNotices(BoardPathExpression.ExpectNotice(BoardPath("BOARD"))),
      json"""
        {
          "TYPE": "ExpectNotice",
          "boardPath": "BOARD"
        }""")
  }
}
