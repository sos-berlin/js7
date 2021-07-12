package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.board.BoardPath
import js7.data.workflow.Instruction
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec
import Instructions.jsonCodec

final class ReadNoticeTest extends AnyFreeSpec
{
  "JSON" in {
    testJson[Instruction](
      ReadNotice(BoardPath("BOARD")),
      json"""
        {
          "TYPE": "ReadNotice",
          "boardPath": "BOARD"
        }""")
  }
}
