package com.sos.jobscheduler.master.gui.components.workfloworders

import com.sos.jobscheduler.data.workflow.instructions.{ForkJoin, IfReturnCode, ImplicitEnd}
import com.sos.jobscheduler.data.workflow.{Instruction, Position, Workflow}
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
import scala.collection.mutable
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
        val sequence = content.workflowToOrderSeq.getOrElse(props.preparedWorkflow.path, Vector.empty)
        <.div(
          <.div(^.cls := "sheet-headline")(
            props.preparedWorkflow.path),
          <.div(sequence.length.toString, " orders"),
          ordersState.error.whenDefined(error ⇒ <.span(^.cls := "error")(error)),
          renderWorkflowContent(props.preparedWorkflow, content))
    }
  }

  private def renderWorkflowContent(preparedWorkflow: PreparedWorkflow, content: FetchedContent) = {
    val (nodesVdom, instructionsWithY) = renderInstructions(preparedWorkflow)
    <.div(^.cls := "Workflow-content",
      nodesVdom,
      renderOrders(preparedWorkflow, instructionsWithY, content))
  }

  private def renderInstructions(preparedWorkflow: PreparedWorkflow): (TagMod, Seq[(Position, Int)]) = {
    val instructionsWithY = mutable.Buffer[(Position, Int)]()
    var y = 0
    def renderWorkflow(workflow: Workflow, parents: List[Position.Parent], depth: Int, stripImplicitEnd: Boolean = false): Seq[TagMod] = {
      var numberedInstructions = workflow.numberedInstructions
      if (stripImplicitEnd && numberedInstructions.last._2.instruction == ImplicitEnd) {
        numberedInstructions = numberedInstructions dropRight 1
      }
      for ((nr, labeled) ← numberedInstructions) yield {
        val position = Position(parents, nr)
        val instructionVdom =
          <.div(^.cls := "orders-Instruction", moveElement(instructionXpx(depth), y))(
            InstructionComponent(labeled))
        instructionsWithY += position → y
        (instructionVdom +:
          (labeled.instruction match {
            case ForkJoin(branches) ⇒
              y += ForkHeight
              branches flatMap { branch ⇒
                val b = <.div(^.cls := "orders-Branch", moveElement(instructionXpx(depth + 1), y), branch.id.string)
                y += BranchHeight
                b +: renderWorkflow(branch.workflow, parents ::: Position.Parent(nr, branch.id) :: Nil, depth + 2)
              }

            case IfReturnCode(_, thenWorkflow, elseOption) ⇒
              y += InstructionHeight
              val then_ = renderWorkflow(thenWorkflow, parents ::: Position.Parent(nr, 0) :: Nil, depth + 1, stripImplicitEnd = true)
              val else_ = elseOption match {
                case Some(elseWorkflow) ⇒
                  val a = <.div(^.cls := "orders-Branch", moveElement(instructionXpx(depth), y), "else")
                  y += ElseHeight
                  a +: renderWorkflow(elseWorkflow, parents ::: Position.Parent(nr, 0) :: Nil, depth + 1, stripImplicitEnd = true)
                case None ⇒
                  Nil
              }
              then_ ++ else_

            case _ ⇒
              y += InstructionHeight
              Nil
        })).toTagMod
      }
    }
    (renderWorkflow(preparedWorkflow.workflow, Nil, 0).toTagMod, instructionsWithY.toVector)
  }

  private def renderOrders(preparedWorkflow: PreparedWorkflow, instructionsWithY: Seq[(Position, Int)], content: FetchedContent) =
    (for {
        (position, y) ← instructionsWithY
        orderIds = content.workflowPositionToOrderIdSeq(preparedWorkflow.path /: position)
        n = min(orderIds.length, OrderPerInstructionLimit)
        (orderEntry, i) ← Array.tabulate[OrderEntry](n)(i ⇒ content.idToEntry(orderIds(i))).zipWithIndex
        x = orderXpx(position.depth, i)
      } yield (orderEntry, x, y)
    ).sortBy(_._1.id)  // Sort to allow React to identify known orders
      .toVdomArray { case (orderEntry, x, y) ⇒
        <.div(^.key := orderEntry.id.string, moveElement(x, y), ^.cls := "orders-Order-moving",
          boxedOrderComponent(orderEntry))
      }
}

object WorkflowOrdersBackend {
  private val OrderPerInstructionLimit = 20
  private val InstructionHeight = 50
  private val BranchHeight = 20
  private val ElseHeight = 20
  private val ForkHeight = InstructionHeight - BranchHeight

  private def instructionXpx(depth: Int) = depthPx(depth)

  private def orderXpx(depth: Int, i: Int) = depthPx(depth) + 215 + 240*i

  private def depthPx(nesting: Int) = 35 * nesting

  private val moveElement = memoize[(Int, Int), TagMod] { case (x, y) ⇒
    ^.transform := s"translate(${x}px,${y}px)"
  }

  private case class Line(y: Int, content: Line.Content)
  private object Line {
    sealed trait Content
    final case class Instr(labeledInstruction: Instruction.Labeled) extends Content
  }
}
