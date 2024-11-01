package js7.data.workflow.position

import js7.base.test.OurTestSuite
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.CycleState
import js7.data.workflow.instructions
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.DynamicPositionTest.*

final class DynamicPositionTest extends OurTestSuite:

  private val dynamicPosition = DynamicPosition(
    InstructionNr(12),
    BranchStack.fromReverseList(List(
      BranchStack.Segment(11, NewBranchId.ForkList),
      BranchStack.Segment(10, NewBranchId.Fork(instructions.ForkBranchId("A"))),
      BranchStack.Segment(9, NewBranchId.Lock),
      BranchStack.Segment(8, NewBranchId.StickySubagent),
      BranchStack.Segment(7, NewBranchId.ConsumeNotices),
      BranchStack.Segment(6, NewBranchId.Cycle(CycleState(
        end = Timestamp("2021-09-30T00:00:00Z"), index = 7, next = Timestamp.Epoch))),
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
    assert(dynamicPosition.toStatic == StaticPositionTest.staticPosition)


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
      10 / "fork+A" %
      11 / "fork" %
      12
