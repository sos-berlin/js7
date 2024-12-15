package js7.data.workflow.position

import js7.base.test.OurTestSuite
import js7.base.time.Timestamp
import js7.base.time.TimestampForTests.ts
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.CycleState
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.DynamicPositionTest.*

final class DynamicPositionTest extends OurTestSuite:

  private val dynamicPosition = DynamicPosition(
    InstructionNr(12),
    BranchStack.fromReverseList(List(
      //BranchStack.Segment(11, NewBranchId.ForkList(instructions.ForkBranchId("1"))),
      //BranchStack.Segment(10, NewBranchId.Fork(instructions.ForkBranchId("A"))),
      BranchStack.Segment(9, NewBranchId.Lock),
      BranchStack.Segment(8, NewBranchId.StickySubagent),
      BranchStack.Segment(7, NewBranchId.ConsumeNotices),
      BranchStack.Segment(6, NewBranchId.Cycle(CycleState(
        end = ts"2021-09-30T00:00:00Z", index = 7, next = Timestamp.Epoch))),
      BranchStack.Segment(5, NewBranchId.Catch(66)),
      BranchStack.Segment(4, NewBranchId.Try(55)),
      BranchStack.Segment(3, NewBranchId.Else),
      BranchStack.Segment(2, NewBranchId.Then()),
      BranchStack.Segment(1, NewBranchId.Options))))

  "fromLegacy" in:
    assert(DynamicPosition.fromLegacy(Position(1)).orThrow == DynamicPosition(InstructionNr(1)))
    assert(DynamicPosition.fromLegacy(Position(1)).orThrow.toLegacy == Position(1))

    val dynPos = DynamicPosition.fromLegacy(legacyPosition).orThrow
    assert(dynPos == dynamicPosition)
    assert(dynPos.toLegacy == legacyPosition)

  "toStatic" in:
    assert(dynamicPosition.toStatic == staticPosition)


object DynamicPositionTest:

  private val legacyPosition =
    Position(1) / "options" %
      2 / "then" %
      3 / "else" %
      4 / "try+55" %
      5 / "catch+66" %
      6 / "cycle+end=1632960000000,i=7" %
      7 / "consumeNotices" %
      8 / "stickySubagent" %
      9 / "lock" %
      //TODO Not decidable, should be a Fork BranchId:     10 / "fork+A" %
      //TODO Not decidable, should be a ForkList BranchId: 11 / "fork+1" %
      12


  // TODO Duplicate to StaticPositionTest.staticPosition, but without non-decidabel Fork and ForkList
  val staticPosition = StaticPosition(
    InstructionNr(12),
    BlockNesting.fromReverseList(List(
      //BlockNesting.Segment(11, BlockNesting.BlockId.ForkList),
      //BlockNesting.Segment(10, BlockNesting.BlockId.Fork(instructions.ForkBranchId("A"))),
      BlockNesting.Segment(9, BlockNesting.BlockId.Lock),
      BlockNesting.Segment(8, BlockNesting.BlockId.StickySubagent),
      BlockNesting.Segment(7, BlockNesting.BlockId.ConsumeNotices),
      BlockNesting.Segment(6, BlockNesting.BlockId.Cycle),
      BlockNesting.Segment(5, BlockNesting.BlockId.Catch),
      BlockNesting.Segment(4, BlockNesting.BlockId.Try),
      BlockNesting.Segment(3, BlockNesting.BlockId.Else),
      BlockNesting.Segment(2, BlockNesting.BlockId.Then),
      BlockNesting.Segment(1, BlockNesting.BlockId.Options))))
