package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.board.BoardPathExpression.{And, ExpectNotice, Or}
import js7.data.board.{BoardPath, BoardPathExpression}
import js7.data.workflow.instructions.ExpectOrConsumeNoticesInstruction.WhenNotAnnounced
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.{Instruction, Workflow}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class ConsumeNoticesTest extends OurTestSuite:

  "JSON" in:
    testJson[Instruction](
      ConsumeNotices(
        Or(
          ExpectNotice(BoardPath("A")),
          And(
            ExpectNotice(BoardPath("B")),
            ExpectNotice(BoardPath("C")))),
        whenNotAnnounced = WhenNotAnnounced.SkipWhenNoNotice,
        Workflow.empty),
      json"""
        {
          "TYPE": "ConsumeNotices",
          "boardPaths": "'A' || 'B' && 'C'",
          "whenNotAnnounced": "SkipWhenNoNotice",
          "subworkflow": {
            "instructions": []
          }
        }""")

    testJsonDecoder[Instruction](
      ConsumeNotices(
        ExpectNotice(BoardPath("A")),
        subworkflow = Workflow.empty),
      json"""
        {
          "TYPE": "ConsumeNotices",
          "boardPaths": "'A'",
          "subworkflow": {
            "instructions": []
          }
        }""")
