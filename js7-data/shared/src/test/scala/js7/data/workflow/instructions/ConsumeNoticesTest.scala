package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.board.{BoardPath, BoardPathExpression}
import js7.data.workflow.{Instruction, Workflow}
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.testJson

final class ConsumeNoticesTest extends OurTestSuite
{
  "JSON" in {
    testJson[Instruction](
      ConsumeNotices(
        BoardPathExpression.Or(
          BoardPathExpression.ExpectNotice(BoardPath("A")),
          BoardPathExpression.And(
            BoardPathExpression.ExpectNotice(BoardPath("B")),
            BoardPathExpression.ExpectNotice(BoardPath("C")))),
        Workflow.empty),
      json"""
        {
          "TYPE": "ConsumeNotices",
          "boardPaths": "'A' || 'B' && 'C'",
          "subworkflow": {
          "instructions": []
          }
        }""")
  }
}
