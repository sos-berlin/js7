package com.sos.jobscheduler.master.gui.browser.common

import com.sos.jobscheduler.data.workflow.Instruction.@:
import com.sos.jobscheduler.data.workflow.instructions.ForkJoin
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
object FlatWorkflows
{
  def flattenWorkflow(workflow: Workflow): Seq[(Position, Instruction.Labeled)] =
    toFlats(workflow, Nil)

  private def toFlats(workflow: Workflow, parents: List[Position.Parent]): Seq[(Position, Instruction.Labeled)] =
    positionAndInstructions(workflow, parents)
      .collect {
        case pi @ (pos, _ @: ForkJoin(branches)) ⇒
          Vector(pi) ++
            (for {
              branch ← branches
              flats ← toFlats(branch.workflow, pos.parents ::: Position.Parent(pos.nr, branch.id) :: Nil)
            } yield flats)

        case o ⇒
          Vector(o)
      }.flatten

  private def positionAndInstructions(workflow: Workflow, parents: List[Position.Parent]): Seq[(Position, Instruction.Labeled)] =
    for ((nr, s) ← workflow.numberedInstructions) yield Position(parents, nr) → s
}
