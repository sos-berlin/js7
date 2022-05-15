package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.board.BoardPath
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class PostNoticesTest extends AnyFreeSpec
{
  "JSON" in {
    testJson[Instruction](
      PostNotices(Seq(BoardPath("BOARD"))),
      json"""
        {
          "TYPE": "PostNotices",
          "boardPaths": [ "BOARD" ]
        }""")
  }

  "JSON with duplicates is rejected" in {
    val wrongJson = json"""{
      "TYPE": "PostNotices",
      "boardPaths": [ "BOARD", "BOARD" ]
    }"""
    assert(wrongJson.as[PostNotices].left.map(_.message) ==
      Left("Unexpected duplicates: 2×Board:BOARD"))
  }
}
