package js7.data.workflow.position

import js7.base.problem.Checked
import js7.data.workflow.position.BranchPath.syntax.*

final case class DynamicPosition(nr: InstructionNr, branchStack: BranchStack = BranchStack.empty):

  def toStatic: StaticPosition =
    StaticPosition(nr, branchStack.toStatic)

  def toLegacy: Position =
    branchStack.toLegacy % nr


object DynamicPosition:

  def fromLegacy(position: Position): Checked[DynamicPosition] =
    BranchStack.fromLegacy(position.branchPath).map: stack =>
      DynamicPosition(position.nr, stack)
