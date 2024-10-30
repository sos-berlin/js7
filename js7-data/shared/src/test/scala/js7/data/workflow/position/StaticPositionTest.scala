package js7.data.workflow.position

import js7.base.test.OurTestSuite
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.workflow.instructions
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.StaticPositionTest.*

final class StaticPositionTest extends OurTestSuite:

  "fromLegacy" in:
    assert(StaticPosition.fromLegacy(Position(1)).orThrow == StaticPosition(InstructionNr(1)))
    assert(StaticPosition.fromLegacy(Position(1)).orThrow.toLegacy == Position(1))

    val statPos = StaticPosition.fromLegacy(legacyPosition).orThrow
    assert(statPos == staticPosition)
    assert(statPos.toLegacy == legacyPosition)


object StaticPositionTest:

  private[position] val staticPosition = StaticPosition(
    InstructionNr(12),
    BlockNesting.fromReverseList(List(
      BlockNesting.Segment(11, BlockNesting.BlockId.ForkList),
      BlockNesting.Segment(10, BlockNesting.BlockId.Fork(instructions.ForkBranchId("A"))),
      BlockNesting.Segment(9, BlockNesting.BlockId.Lock),
      BlockNesting.Segment(8, BlockNesting.BlockId.StickySubagent),
      BlockNesting.Segment(7, BlockNesting.BlockId.ConsumeNotices),
      BlockNesting.Segment(6, BlockNesting.BlockId.Cycle),
      BlockNesting.Segment(5, BlockNesting.BlockId.Catch),
      BlockNesting.Segment(4, BlockNesting.BlockId.Try),
      BlockNesting.Segment(3, BlockNesting.BlockId.Else),
      BlockNesting.Segment(2, BlockNesting.BlockId.Then),
      BlockNesting.Segment(1, BlockNesting.BlockId.Options))))

  private val legacyPosition =
    Position(1) / "options" %
      2 / "then" %
      3 / "else" %
      4 / "try" %
      5 / "catch" %
      6 / "cycle" %
      7 / "consumeNotices" %
      8 / "stickySubagent" %
      9 / "lock" %
      10 / "fork+A" %
      11 / "fork" %
      12
