package com.sos.jobscheduler.master.gui.components.workfloworders

import com.sos.jobscheduler.master.gui.components.state.{OrdersState, PreparedWorkflow}
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.StateSnapshot

/**
  * @author Joacim Zschimmer
  */
object WorkflowOrdersComponent {

  def apply(workflow: PreparedWorkflow, stateSnapshot: StateSnapshot[OrdersState])
  : Unmounted[Props, Unit, WorkflowOrdersBackend] =
    scalaComponent(Props(workflow, stateSnapshot))

  private val scalaComponent = ScalaComponent.builder[Props]("OrderList")
    .renderBackend[WorkflowOrdersBackend]
    .build

  final case class Props(preparedWorkflow: PreparedWorkflow, ordersStateSnapshot: StateSnapshot[OrdersState])
}
