package com.sos.jobscheduler.master.gui.components.orderlist.workflowlist

import com.sos.jobscheduler.data.workflow.{WorkflowPath, WorkflowScript}
import com.sos.jobscheduler.master.gui.router.Router
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{BackendScope, ScalaComponent}

/**
  * @author Joacim Zschimmer
  */
object WorkflowListComponent {

  def apply(pathToWorkflow: Map[WorkflowPath, WorkflowScript]) =
    scalaComponent(pathToWorkflow)

  private val scalaComponent = ScalaComponent.builder[Props]("WorkflowList")
    .renderBackend[Backend]
    .build

  final class Backend(scope: BackendScope[Props, Unit]) {
    def render(pathToWorkflow: Props): VdomElement =
      <.div(
        <.div(^.cls := "sheet-headline")(
          s"${pathToWorkflow.size} workflows"),
        pathToWorkflow.keys.toVector.sorted.toVdomArray(path ⇒
          <.a(^.cls := "hidden-link", ^.href := Router.hash(path))(path.string)))
  }

  private type Props = Map[WorkflowPath, WorkflowScript]
}
