package js7.data.workflow.instructions

import js7.base.problem.Checked
import js7.base.utils.Collections.implicits.RichIterable
import js7.data.board.BoardPath
import js7.data.workflow.Instruction

trait BoardInstruction extends Instruction
{
  def boardPaths: Vector[BoardPath]

  protected def checked: Checked[this.type] =
    for (_ <- boardPaths.checkUniqueness) yield this
}
