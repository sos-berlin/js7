package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.board.BoardPathExpression.{And, ExpectNotice, Or}
import js7.data.board.{BoardPath, BoardPathExpression}
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.ExpectOrConsumeNoticesInstruction.WhenNotAnnounced
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class ExpectNoticesTest extends OurTestSuite:

  "JSON" in:
    testJson[Instruction](
      ExpectNotices(
        Or(
          ExpectNotice(BoardPath("A")),
          And(
            ExpectNotice(BoardPath("B")),
            ExpectNotice(BoardPath("C")))),
        whenNotAnnounced = WhenNotAnnounced.SkipWhenNoNotice),
      json"""
        {
          "TYPE": "ExpectNotices",
          "boardPaths": "'A' || 'B' && 'C'",
          "whenNotAnnounced": "SkipWhenNoNotice"
        }""")

    testJsonDecoder[Instruction](
      ExpectNotices(ExpectNotice(BoardPath("A"))),
      json"""
        {
          "TYPE": "ExpectNotices",
          "boardPaths": "'A'"
        }""")
