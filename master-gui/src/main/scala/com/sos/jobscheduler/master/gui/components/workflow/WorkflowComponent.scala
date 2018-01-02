package com.sos.jobscheduler.master.gui.components.workflow

import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.workflow.WorkflowScript._
import com.sos.jobscheduler.data.workflow.{AgentJobPath, WorkflowPath, WorkflowScript}
import com.sos.jobscheduler.master.gui.common.Renderers._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
object WorkflowComponent {

  def apply(workflow: WorkflowScript.Named, orders: Seq[Order[Order.State]] = Nil) =
    scalaComponent(Props(workflow, orders))

  private val scalaComponent = ScalaComponent.builder[Props]("Workflow")
    .render_P {
      case Props(workflow, Seq()) ⇒ renderWorkflow(workflow)
      case Props(workflow, orders) ⇒ renderWorkflowWithOrder(workflow, orders)
    }
    .build

  private def renderWorkflowWithOrder(workflow: WorkflowScript.Named, orders: Seq[Order[Order.State]]) =
    <.table(^.cls := "no-padding")(
      <.colgroup(<.col(^.cls := "Workflow-symbol-col"), <.col),
      <.tbody(
        <.tr(<.td, renderHeadlineTh(workflow.path)),
        workflow.script.flatten.toVdomArray(flat ⇒
          (for {
             nodeId ← flat.nodeIdOption
             order ← orders collectFirst { case order if order.nodeId == nodeId ⇒ order }
           } yield
             <.tr(
               <.td(<.div(^.cls := "WorkflowScript-Statement", outcomeSymbol(order.outcome), orderStateToSymbol(order.state))),
               <.td(<.div(^.cls := stateToClass(order.state), renderFlatStatement(flat)))): VdomNode
          ) getOrElse <.tr(<.td, renderFlatStatementTd(flat)))))

  private def renderWorkflow(workflow: WorkflowScript.Named): VdomElement =
    <.table(^.cls := "no-padding")(  // Same layout as renderWorkflowWithOrder
      <.tbody(
        <.tr(renderHeadlineTh(workflow.path)),
        workflow.script.flatten.toVdomArray(flat ⇒
          <.tr(renderFlatStatementTd(flat)))))

  private def renderHeadlineTh(workflowPath: WorkflowPath) =
    <.td(
      <.h5(^.cls := "WorkflowScript-Statement")(
        "Workflow ", <.span(^.whiteSpace := "nowrap")(workflowPath.string)))

  private def stateToClass(state: Order.State): String =
    state match {
      case Order.InProcess          ⇒ "WorkflowScript-Statement WorkflowScript-Statement-InProcess"
      case Order.Ready              ⇒ "WorkflowScript-Statement WorkflowScript-Statement-Ready"
      case _: Order.Transitionable  ⇒ "WorkflowScript-Statement WorkflowScript-Statement-Transitionable"
      case Order.Finished           ⇒ "WorkflowScript-Statement WorkflowScript-Statement-Finished"
      case _                        ⇒ "WorkflowScript-Statement WorkflowScript-Statement-Idle"
    }

  private def renderFlatStatementTd(flat: FlatStatement) =
    <.td(<.div(^.cls := "WorkflowScript-Statement", renderFlatStatement(flat)))

  private def renderFlatStatement(stmt: FlatStatement): VdomNode =
    stmt match {
      case FlatStatement.Node(nesting, Job(nodeId, AgentJobPath(agentPath, jobPath))) ⇒
        VdomArray(nesting.toString, " ", nodeId, ": ", agentPath, " · ", jobPath)

      case FlatStatement.Node(nesting, End(nodeId)) ⇒
        VdomArray(nesting.toString, " ", nodeId, ": ", "end")

      case FlatStatement.NonNode(nesting, IfError(nodeId)) ⇒
        VdomArray(nesting.toString, " ", "onError ", nodeId)

      case FlatStatement.NonNode(nesting, Goto(nodeId)) ⇒
        VdomArray(nesting.toString, " ", "goto ", nodeId)
    }

  final case class Props(workflow: WorkflowScript.Named, orders: Seq[Order[Order.State]])
}
