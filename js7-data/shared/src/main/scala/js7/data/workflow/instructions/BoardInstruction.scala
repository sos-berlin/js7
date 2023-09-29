package js7.data.workflow.instructions

import js7.data.board.BoardPath
import js7.data.workflow.Instruction

trait BoardInstruction extends Instruction:
  def referencedBoardPaths: Set[BoardPath]
