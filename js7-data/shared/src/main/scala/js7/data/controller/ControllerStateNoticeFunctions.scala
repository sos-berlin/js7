package js7.data.controller

import js7.base.problem.{Checked, Problem}
import js7.data.board.BoardPath
import js7.data.state.EngineStateFunctions
import js7.data.workflow.instructions.NoticeInstruction
import js7.data.workflow.position.WorkflowPosition
import scala.reflect.ClassTag

trait ControllerStateNoticeFunctions extends EngineStateFunctions:

  // COMPATIBLE with v2.3
  final def workflowPositionToBoardPath(workflowPosition: WorkflowPosition): Checked[BoardPath] =
    instruction_[NoticeInstruction](workflowPosition).flatMap:
      _.referencedBoardPaths.toSeq match
        case Seq(boardPath) => Right(boardPath)
        case _ => Left(Problem.pure:
          "Legacy orderIdToBoardState, but instruction has multiple BoardPaths")
