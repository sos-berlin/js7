package com.sos.jobscheduler.master.gui.browser.components.state

import com.sos.jobscheduler.data.workflow.instructions.{ForkJoin, If, ImplicitEnd}
import com.sos.jobscheduler.data.workflow.position.{BranchId, BranchPath, InstructionNr, Position}
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow, WorkflowId}
import com.sos.jobscheduler.master.gui.browser.components.state.PreparedWorkflow._
import com.sos.jobscheduler.master.gui.browser.components.workfloworders.WorkflowComponent.moveElement
import japgolly.scalajs.react.vdom.html_<^._
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final case class PreparedWorkflow(id: WorkflowId, workflow: Workflow)
{
  lazy val positionsWithXY: Seq[(Position, Int, Int)] = PositionOnlyScaffoldBuilder.build(workflow)
}

object PreparedWorkflow {
  trait ScaffoldBuilder {
    def addVdom(vdomNode: ⇒ VdomNode): Unit
    def instructionToVdom(labeled: Instruction.Labeled): VdomNode

    def build(workflow: Workflow): Vector[(Position, Int, Int)] = {
      val positionsWithXY = Vector.newBuilder[(Position, Int, Int)]
      var lastY = 0
      // Same procedure as in WorkflowComponent
      def renderNested(labeledInstructions: Seq[Instruction.Labeled], parents: BranchPath, nesting: Int): Unit = {
        for ((labeled, nr) ← labeledInstructions.zipWithIndex) {
          val y = lastY
          lastY += InstructionHeight
          val position = parents / InstructionNr(nr)
          positionsWithXY += ((position, nestingPx(nesting), y))
          addVdom(<.div(^.cls := "orders-Instruction", moveElement(nestingPx(nesting), y), instructionToVdom(labeled)))
          labeled.instruction match {
            case ForkJoin(branches) ⇒
              for (branch ← branches) {
                addVdom(<.div(^.cls := "orders-Branch", moveElement(nestingPx(nesting + 0.4), lastY - InstructionHeight + BranchIdY), branch.id.string))
                renderNested(branch.workflow.labeledInstructions, position / branch.id, nesting + 1)
              }

            case _if: If ⇒
              def renderBranch(branch: BranchId.Indexed) = for (w ← _if.workflow(branch.number))  // always Valid
                renderNested(stripImplicitEnd(w.labeledInstructions), position / branch, nesting + 1)
              renderBranch(0)  // then
              if (_if.elseWorkflow.isDefined) {
                addVdom(<.div(^.cls := "orders-Branch", moveElement(nestingPx(nesting), lastY - InstructionHeight + ElseY), "else"))
                renderBranch(1)  // else
              }

            case _ ⇒
          }
        }
      }
      renderNested(workflow.labeledInstructions, BranchPath.Empty, nesting = 0)
      positionsWithXY.result()
    }
  }

  private object PositionOnlyScaffoldBuilder extends ScaffoldBuilder {
    def addVdom(vdomNode: ⇒ VdomNode) = {}
    def instructionToVdom(labeled: Instruction.Labeled) = throw new NotImplementedError
  }

  private val InstructionHeight = 42
  private val BranchIdY = 30
  private val ElseY = 32

  def nestingPx(nesting: Double) = (35 * nesting).toInt

  def stripImplicitEnd(numbered: Seq[Instruction.Labeled]): Seq[Instruction.Labeled] =
    if (numbered.last.instruction == ImplicitEnd)
      numbered dropRight 1
    else
      numbered
}
