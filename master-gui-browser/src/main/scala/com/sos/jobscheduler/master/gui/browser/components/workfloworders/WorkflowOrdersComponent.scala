package com.sos.jobscheduler.master.gui.browser.components.workfloworders

import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.master.gui.browser.components.state.OrdersState.{FetchedContent, Mark}
import com.sos.jobscheduler.master.gui.browser.components.state.{OrdersState, PreparedWorkflow}
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.{Callback, ScalaComponent}

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
  {
    def onMouseOver(orderId: OrderId): Callback =
      ordersStateSnapshot
        .modState(state ⇒
          state.content match {
            case content: FetchedContent if !content.isMarked(orderId, Mark.Permanent) ⇒
              state.copy(content = content.markOrder(orderId, Mark.Volatile))
            case _ ⇒ state
          })

    def onMouseOut(orderId: OrderId): Callback =
      ordersStateSnapshot
        .modState(state ⇒
          state.content match {
            case content: FetchedContent if content.isMarked(orderId, Mark.Volatile) ⇒
              state.copy(content = content.unmarkOrder(orderId, Mark.Volatile))
            case _ ⇒ state
          })

    def onClick(orderId: OrderId): Callback =
      ordersStateSnapshot
        .modState(state ⇒
          state.content match {
            case content: FetchedContent ⇒
              state.copy(content =
                if (content.isMarked(orderId, Mark.Permanent))
                  content.unmarkOrder(orderId, Mark.Permanent)
                else
                  content.markOrder(orderId, Mark.Permanent))
            case _ ⇒ state
          })
  }
}
