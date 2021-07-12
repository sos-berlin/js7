package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.board.BoardPath
import js7.data.workflow.Instruction
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec
import Instructions.jsonCodec

final class PostNoticeTest extends AnyFreeSpec
{
  "JSON" in {
    testJson[Instruction](
      PostNotice(BoardPath("BOARD")),
      json"""
        {
          "TYPE": "PostNotice",
          "boardPath": "BOARD"
        }""")
  }
}
