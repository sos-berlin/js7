package com.sos.jobscheduler.master.gui.components.orderlist

import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.gui.components.state.{OrdersState, PreparedWorkflow}
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.{EventListener, StateSnapshot}
import org.scalajs.dom.MouseEvent

/**
  * @author Joacim Zschimmer
  */
object OrderListComponent {

  def apply(workflowPath: WorkflowPath, workflow: PreparedWorkflow, stateSnapshot: StateSnapshot[OrdersState]): Unmounted[Props, Unit, OrderListBackend] =
    scalaComponent(Props(workflowPath, workflow, stateSnapshot))

  private val scalaComponent = ScalaComponent.builder[Props]("OrderList")
    .renderBackend[OrderListBackend]
    .componentDidUpdate(o ⇒ o.backend.componentDidUpdate(o))
    .configure(EventListener[MouseEvent].install("mousemove", _.backend.onMouseMoved))
    .build

  final case class Props(workflowPath: WorkflowPath, workflow: PreparedWorkflow, ordersStateSnapshot: StateSnapshot[OrdersState])
}
