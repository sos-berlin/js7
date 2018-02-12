package com.sos.jobscheduler.master.gui.components.workflow

import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.workflow.instructions._
import com.sos.jobscheduler.data.workflow.{Instruction, Position, WorkflowPath}
import com.sos.jobscheduler.master.gui.common.Renderers._
import com.sos.jobscheduler.master.gui.components.state.PreparedWorkflow
import com.sos.jobscheduler.master.gui.common.FlatWorkflows.flattenWorkflow
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
object WorkflowComponent {

  def apply(workflow: PreparedWorkflow, orders: Seq[Order[Order.State]] = Nil) =
    scalaComponent(Props(workflow, orders))

  private val scalaComponent = ScalaComponent.builder[Props]("Workflow")
    .render_P {
      case Props(workflow, Seq()) ⇒ renderWorkflow(workflow)
      case Props(workflow, orders) ⇒ renderWorkflowWithOrders(workflow, orders)
    }
    .build

  private def renderWorkflow(preparedWorkflow: PreparedWorkflow): VdomElement =
    <.table(^.cls := "no-padding")(  // Same layout as renderWorkflowWithOrders
      <.tbody(
        <.tr(renderHeadlineTh(preparedWorkflow.path)),
        flattenWorkflow(preparedWorkflow.workflow).toVdomArray(flat ⇒
          <.tr(renderInstructionTd(flat)))))

  private def renderWorkflowWithOrders(preparedWorkflow: PreparedWorkflow, orders: Seq[Order[Order.State]]) =
    <.table(^.cls := "no-padding")(
      <.colgroup(<.col(^.cls := "Workflow-symbol-col"), <.col),
      <.tbody(
        <.tr(<.td, renderHeadlineTh(preparedWorkflow.path)),
        flattenWorkflow(preparedWorkflow.workflow).toVdomArray(posInstr ⇒
          (for {
             order ← orders collectFirst { case order if order.position == posInstr._1 ⇒ order }
           } yield
             <.tr(
               <.td(<.div(^.cls := "Instruction", orderStateToSymbol(order.state))),
               <.td(<.div(^.cls := stateToClass(order.state), renderInstruction(posInstr)))): VdomNode
          ) getOrElse <.tr(<.td, renderInstructionTd(posInstr)))))

  private def renderHeadlineTh(workflowPath: WorkflowPath) =
    <.td(
      <.h5(^.cls := "Instruction")(
        "Workflow ", <.span(^.whiteSpace := "nowrap")(workflowPath.string)))

  private def stateToClass(state: Order.State): String =
    state match {
      case Order.InProcess          ⇒ "Instruction Instruction-InProcess"
      case Order.Ready              ⇒ "Instruction Instruction-Ready"
      case _: Order.Transitionable  ⇒ "Instruction Instruction-Transitionable"
      case Order.Finished           ⇒ "Instruction Instruction-Finished"
      case _                        ⇒ "Instruction Instruction-Idle"
    }

  private def renderInstructionTd(pi: (Position, Instruction.Labeled)) =
    <.td(<.div(^.cls := "Instruction", renderInstruction(pi)))

  private def renderInstruction(pi: (Position, Instruction.Labeled)): VdomNode =
    VdomArray(
      pi._1.parents map (_.branchId) mkString "/",
      " ",
      pi._2.labels.map(_ + ": ").mkString,
      pi._2.instruction match {
        case job: Job ⇒
          VdomArray(job.agentPath, " · ", job.jobPath)

        case _: ForkJoin ⇒
          "fork"

        case _: End ⇒
          "end"

        case stmt ⇒
          stmt.toShortString
      })

  final case class Props(preparedWorkflow: PreparedWorkflow, orders: Seq[Order[Order.State]])
}
