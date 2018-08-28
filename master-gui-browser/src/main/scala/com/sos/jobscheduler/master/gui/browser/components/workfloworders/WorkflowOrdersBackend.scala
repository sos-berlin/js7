package com.sos.jobscheduler.master.gui.browser.components.workfloworders

import com.sos.jobscheduler.master.gui.browser.common.Renderers._
import com.sos.jobscheduler.master.gui.browser.common.Utils.memoize
import com.sos.jobscheduler.master.gui.browser.components.state.OrdersState.{FetchedContent, FetchingContent, OrderEntry, Starting}
import com.sos.jobscheduler.master.gui.browser.components.state.PreparedWorkflow
import com.sos.jobscheduler.master.gui.browser.components.workfloworders.WorkflowOrdersBackend._
import com.sos.jobscheduler.master.gui.browser.components.workfloworders.WorkflowOrdersComponent.Props
import japgolly.scalajs.react.BackendScope
import japgolly.scalajs.react.extra.OnUnmount
import japgolly.scalajs.react.vdom.html_<^._
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
      case Starting ⇒
        <.div

      case FetchingContent ⇒
        <.div(<.i("Fetching orders..."))

      case content: FetchedContent ⇒
        val sequence = content.workflowToOrderIds.getOrElse(props.preparedWorkflow.id, Vector.empty)
        <.div(
          <.div(^.cls := "sheet-headline")(
            props.preparedWorkflow.id),
          <.div(sequence.length.toString, " orders"),
          ordersState.error.whenDefined(error ⇒ <.span(^.cls := "error")(error)),
          <.div(^.cls := "Workflow-with-orders",
            WorkflowComponent(props.preparedWorkflow),
            renderOrders(props.preparedWorkflow, content)))
    }
  }

  private def renderOrders(preparedWorkflow: PreparedWorkflow, content: FetchedContent) =
    <.div(
      (for {
          (position, x0, y) ← preparedWorkflow.positionsWithXY
          orderIds = content.workflowPositionToOrderIdSeq(preparedWorkflow.id /: position)
          n = min(orderIds.length, OrderPerInstructionLimit)
          (orderEntry, i) ← Iterator.tabulate[(OrderEntry, Int)](n)(i ⇒ content.idToEntry(orderIds(i)) → i)
          x = orderXpx(x0, i)
        } yield (orderEntry, x, y))
      .sortBy(_._1.id)  // Sort to allow React to identify known orders
      .toVdomArray { case (orderEntry, x, y) ⇒
        <.div(^.key := orderEntry.id.string, ^.cls := "orders-Order-moving", moveElement(x, y),
          boxedOrderComponent(orderEntry))
      })
}

object WorkflowOrdersBackend {
  private val OrderPerInstructionLimit = 20

  private def orderXpx(start: Int, i: Int) = start + 215 + 230*i

  private val moveElement = memoize[(Int, Int), TagMod] { case (x, y) ⇒
    WorkflowComponent.moveElement(x, y)
  }
}
