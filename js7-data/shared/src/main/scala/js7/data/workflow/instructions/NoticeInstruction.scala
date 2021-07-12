package js7.data.workflow.instructions

import js7.data.board.BoardPath
import js7.data.workflow.Instruction

trait NoticeInstruction extends Instruction
{
  def boardPath: BoardPath
}
