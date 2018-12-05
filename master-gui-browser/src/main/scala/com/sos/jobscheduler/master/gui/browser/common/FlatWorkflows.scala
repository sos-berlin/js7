package com.sos.jobscheduler.master.gui.browser.common

import com.sos.jobscheduler.data.workflow.Instruction.@:
import com.sos.jobscheduler.data.workflow.instructions.Fork
import com.sos.jobscheduler.data.workflow.position.{BranchPath, Position}
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
object FlatWorkflows
{
  def flattenWorkflow(workflow: Workflow): Seq[(Position, Instruction.Labeled)] =
    toFlats(workflow, Nil)

  private def toFlats(workflow: Workflow, branchPath: BranchPath): Seq[(Position, Instruction.Labeled)] =
    positionAndInstructions(workflow, branchPath)
      .collect {
        case pi @ (pos, _ @: Fork(branches)) ⇒
          Vector(pi) ++
            (for {
              branch ← branches
              flats ← toFlats(branch.workflow, pos.branchPath / pos.nr / branch.id)
            } yield flats)

        case o ⇒
          Vector(o)
      }.flatten

  private def positionAndInstructions(workflow: Workflow, branchPath: BranchPath): Seq[(Position, Instruction.Labeled)] =
    for ((nr, s) ← workflow.numberedInstructions) yield Position(branchPath, nr) → s
}
