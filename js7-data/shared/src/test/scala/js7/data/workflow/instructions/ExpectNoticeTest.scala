package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.board.BoardPath
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class ExpectNoticeTest extends AnyFreeSpec
{
  "JSON" in {
    testJson[Instruction](
      ExpectNotice(BoardPath("BOARD")),
      json"""
        {
          "TYPE": "ExpectNotice",
          "boardPath": "BOARD"
        }""")
  }
}
