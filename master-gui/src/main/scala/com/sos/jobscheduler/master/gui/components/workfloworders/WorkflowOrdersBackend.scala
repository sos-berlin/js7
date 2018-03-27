package com.sos.jobscheduler.master.gui.components.workfloworders

import com.sos.jobscheduler.data.workflow.Position.BranchId
import com.sos.jobscheduler.data.workflow.instructions.{ForkJoin, If, ImplicitEnd}
import com.sos.jobscheduler.data.workflow.{Instruction, InstructionNr, Position}
import com.sos.jobscheduler.master.gui.common.Renderers._
import com.sos.jobscheduler.master.gui.common.Utils.memoize
import com.sos.jobscheduler.master.gui.components.state.OrdersState._
import com.sos.jobscheduler.master.gui.components.state.PreparedWorkflow
import com.sos.jobscheduler.master.gui.components.workfloworders.WorkflowOrdersBackend._
import com.sos.jobscheduler.master.gui.components.workfloworders.WorkflowOrdersComponent.Props
import japgolly.scalajs.react.BackendScope
import japgolly.scalajs.react.extra.OnUnmount
import japgolly.scalajs.react.vdom.html_<^._
import scala.collection.immutable.Seq
import scala.math.min

/**
  * @author Joacim Zschimmer
  */
private[workfloworders] final class WorkflowOrdersBackend(scope: BackendScope[Props, Unit])
extends OnUnmount {

  private val boxedOrderComponent = new BoxedOrderComponent

  def render(props: Props): VdomElement = {
    val ordersState = props.ordersStateSnapshot.value
    ordersState.content match {
      case Initial ⇒
        <.div

      case FetchingContent ⇒
        <.div(<.i("Fetching orders..."))

      case content: FetchedContent ⇒
        val sequence = content.workflowToOrderSeq.getOrElse(props.preparedWorkflow.id, Vector.empty)
        <.div(
          <.div(^.cls := "sheet-headline")(
            props.preparedWorkflow.id),
          <.div(sequence.length.toString, " orders"),
          ordersState.error.whenDefined(error ⇒ <.span(^.cls := "error")(error)),
          renderWorkflowContent(props.preparedWorkflow, content))
    }
  }

  private def renderWorkflowContent(preparedWorkflow: PreparedWorkflow, content: FetchedContent) = {
    val (nodesVdom, instructionsWithXY) = renderInstructions(preparedWorkflow)
    <.div(^.cls := "Workflow-content",
      nodesVdom,
      renderOrders(preparedWorkflow, instructionsWithXY, content))
  }

  private def renderInstructions(preparedWorkflow: PreparedWorkflow): (TagMod, Seq[(Position, Int, Int)]) = {
    val tagMods = Vector.newBuilder[TagMod]
    val positionsWithXY = Vector.newBuilder[(Position, Int, Int)]
    var y = 0
    def renderNested(numberedInstructions: Seq[(InstructionNr, Instruction.Labeled)], parents: Position.Parents, nesting: Int): Unit = {
      for ((nr, labeled) ← numberedInstructions) {
        val position = parents / nr
        tagMods += <.div(^.cls := "orders-Instruction", moveElement(instructionXpx(nesting), y), InstructionComponent(labeled))
        positionsWithXY += ((position, nestingPx(nesting), y))
        y += InstructionHeight
        labeled.instruction match {
          case ForkJoin(branches) ⇒
            for (branch ← branches) {
              tagMods += <.div(^.cls := "orders-Branch", moveElement(instructionXpx(nesting + 0.4), y - BranchHeight), branch.id.string)
              renderNested(branch.workflow.numberedInstructions, position / branch.id, nesting + 1)
            }

          case _if: If ⇒
            def renderBranch(branch: BranchId.Indexed) = for (w ← _if.workflow(branch.number))  // always Valid
              renderNested(stripImplicitEnd(w.numberedInstructions), position / branch, nesting + 1)
            renderBranch(0)  // then
            if (_if.elseWorkflow.isDefined) {
              tagMods += <.div(^.cls := "orders-Branch", moveElement(instructionXpx(nesting), y + ElseY), "else")
              renderBranch(1)  // else
            }

          case _ ⇒
        }
      }
    }
    renderNested(preparedWorkflow.workflow.numberedInstructions, Position.Parents.Empty, nesting = 0)
    (tagMods.result().toTagMod, positionsWithXY.result())
  }

  private def renderOrders(preparedWorkflow: PreparedWorkflow, instructionsWithXY: Seq[(Position, Int, Int)], content: FetchedContent) =
    (for {
        (position, x0, y) ← instructionsWithXY
        orderIds = content.workflowPositionToOrderIdSeq(preparedWorkflow.id /: position)
        n = min(orderIds.length, OrderPerInstructionLimit)
        (orderEntry, i) ← Array.tabulate[OrderEntry](n)(i ⇒ content.idToEntry(orderIds(i))).zipWithIndex
        x = orderXpx(x0, i)
      } yield (orderEntry, x, y))
    .sortBy(_._1.id)  // Sort to allow React to identify known orders
    .toVdomArray { case (orderEntry, x, y) ⇒
      <.div(^.key := orderEntry.id.string, moveElement(x, y), ^.cls := "orders-Order-moving",
        boxedOrderComponent(orderEntry))
    }
}

object WorkflowOrdersBackend {
  private val OrderPerInstructionLimit = 20
  private val InstructionHeight = 41
  private val BranchHeight = 20
  private val ElseY = -10

  private def instructionXpx(nesting: Double) = nestingPx(nesting)

  private def orderXpx(start: Int, i: Int) = start + 215 + 230*i

  private def nestingPx(nesting: Double) = (35 * nesting).toInt

  private val moveElement = memoize[(Int, Int), TagMod] { case (x, y) ⇒
    ^.transform := s"translate(${x}px,${y}px)"
  }

  private case class Line(y: Int, content: Line.Content)
  private object Line {
    sealed trait Content
    final case class Instr(labeledInstruction: Instruction.Labeled) extends Content
  }

  private def stripImplicitEnd(numbered: Seq[(InstructionNr, Instruction.Labeled)]): Seq[(InstructionNr, Instruction.Labeled)] =
    if (numbered.last._2.instruction == ImplicitEnd)
      numbered dropRight 1
    else
      numbered
}
