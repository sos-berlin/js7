package com.sos.jobscheduler.core.workflow

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.workflow.InstructionContext._
import com.sos.jobscheduler.data.workflow.Instruction
import com.sos.jobscheduler.data.workflow.instructions.{If, TryInstruction}
import com.sos.jobscheduler.data.workflow.position.{BranchPath, Position, WorkflowPosition}
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
trait InstructionContext
{
  def instruction(workflowPosition: WorkflowPosition): Instruction

  final def toRetryCount(workflowPosition: WorkflowPosition): Int =
    workflowPosition.position.splitBranchAndNr.fold(DefaultRetryCount) { case (parentPos, branchId, _) =>
      instruction(workflowPosition.workflowId /: parentPos) match {
        case _: TryInstruction =>
          TryInstruction.toRetryIndex(branchId)
            .onProblemHandle { p =>
              logger.error(s"toRetryCount($workflowPosition) $p")
              DefaultRetryCount   // Invalid BranchId
            }
        case _ =>
          toRetryCount(workflowPosition.workflowId /: parentPos)
      }
    }

  final def toRetryPosition(workflowPosition: WorkflowPosition): Checked[Position] =
    toRetryBranchPath(workflowPosition).map(_ % 0)

  @tailrec
  private def toRetryBranchPath(workflowPosition: WorkflowPosition): Checked[BranchPath] =
    workflowPosition.position.splitBranchAndNr match {
      case None => Invalid(NoTryBlockProblem)
      case Some((parent, branchId, _)) =>
        instruction(workflowPosition.workflowId /: parent) match {
          case _: TryInstruction =>
            TryInstruction.nextTryBranchId(branchId) match {
              case Valid(None) => toRetryBranchPath(workflowPosition.workflowId /: parent)
              case Valid(Some(tryBranchId)) => Valid(parent / tryBranchId)
              case o @ Invalid(_) => o
            }
          case _: If =>
            toRetryBranchPath(workflowPosition.workflowId /: parent)
          case _ =>  // For example, Fork is a barrier. Retry may not be issued inside a Fork for a Try outside the Fork
            Invalid(NoTryBlockProblem)
        }
    }
}

object InstructionContext {
  private val logger = Logger(getClass)
  private val DefaultRetryCount = 0
  private val NoTryBlockProblem = Problem.pure("Not in a catch-block")
}
