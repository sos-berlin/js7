package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.board.BoardPath
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class ExpectNoticesTest extends AnyFreeSpec
{
  "JSON" in {
    testJson[Instruction](
      ExpectNotices(Vector(BoardPath("A"), BoardPath("B"))),
      json"""
        {
          "TYPE": "ExpectNotices",
          "boardPaths": [ "A", "B" ]
        }""")
  }

  "JSON with duplicates is rejected" in {
    val wrongJson = json"""{
      "TYPE": "ExpectNotices",
      "boardPaths": [ "BOARD", "BOARD" ]
    }"""
    assert(wrongJson.as[ExpectNotices].left.map(_.message) ==
      Left("Unexpected duplicates: 2×Board:BOARD"))
  }
}
