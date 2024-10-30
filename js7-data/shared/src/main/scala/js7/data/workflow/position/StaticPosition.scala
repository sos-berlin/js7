package js7.data.workflow.position

import js7.base.problem.Checked
import js7.data.workflow.position.BranchPath.syntax.*

final case class StaticPosition(nr: InstructionNr, blockNesting: BlockNesting = BlockNesting.empty):

  def toLegacy: Position =
    blockNesting.toLegacy % nr


object StaticPosition:

  def fromLegacy(position: Position): Checked[StaticPosition] =
    BlockNesting.fromLegacy(position.branchPath)
      .map: segments =>
        StaticPosition(position.nr, segments)

  final case class Segment(nr: InstructionNr, blockId: BlockNesting.BlockId):
    def toLegacy: BranchPath.Segment =
      BranchPath.Segment(nr, blockId.toLegacyBranchId)
