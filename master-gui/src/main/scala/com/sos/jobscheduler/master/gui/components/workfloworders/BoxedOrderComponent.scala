package com.sos.jobscheduler.master.gui.components.workfloworders

import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.master.gui.common.Renderers.forTable.orderStateToVdom
import com.sos.jobscheduler.master.gui.common.Renderers.orderStateToSymbol
import com.sos.jobscheduler.master.gui.components.state.OrdersState
import com.sos.jobscheduler.master.gui.components.state.OrdersState.OrderEntry
import com.sos.jobscheduler.master.gui.router.Router
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, Ref, ScalaComponent}
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.window

/**
  * @author Joacim Zschimmer
  */
private[workfloworders] final class BoxedOrderComponent
{
  private val orderSelector = new OrderSelector  // Has state
  private val elementRef = Ref[HTMLElement]

  def apply(orderEntry: OrderEntry) = component(orderEntry)

  private val component = ScalaComponent.builder[OrdersState.OrderEntry]("Little-Order")
    .render_P {
      case OrdersState.OrderEntry(order, _, lastOutput, updatedAt) ⇒
        orderSelector.updateOrder(order)
        val selectedClass = orderSelector.cssClass(order.id)
        val highlighted = false //updatedAt > highlighter.lastHighlightedAt && selectedClass.isEmpty
        <.div(^.id := OrderSelector.elementId(order.id),
              ^.cls := "sheet z-depth-1 orders-Order " + orderToClass(order) + selectedClass + (if (highlighted) "orders-Order-changed" else ""),
              ^.title := "Double-click for details",
              ^.onMouseOver --> Callback { orderSelector.onMouseOver(order) },
              ^.onMouseOut --> Callback { orderSelector.onMouseOut(order.id) },
              ^.onClick --> Callback { orderSelector.onClick(order) },
              ^.onDoubleClick --> Callback { window.document.location.assign(Router.hash(order.id)) },
          <.div(^.cls := "orders-Order-OrderId", order.id.string),
          order.state match {
            case _: Order.Fresh | _: Order.Join ⇒
              <.div(^.cls := "orders-Order-compact",
                order.state)

            case Order.InProcess ⇒
              <.div(^.cls := "orders-Order-compact",
                orderStateToSymbol(order.state), " ",
                lastOutput getOrElse order.state.toString: String)

            case _ ⇒
              <.div(
                <.span(^.cls := "orders-Order-State", order.state))
          })
        .withRef(elementRef)
    }
    .configure {
      implicit val orderEntryReuse = Reusability.byRef[OrdersState.OrderEntry]
      Reusability.shouldComponentUpdate
    }
    .build

  private def orderToClass(order: Order[Order.State]): String =
    order.state match {
      case _: Order.Fresh      ⇒ "Order-Fresh "
      case Order.InProcess     ⇒ "Order-InProcess "
      case _: Order.Join       ⇒ "Order-Join "
      case Order.Finished      ⇒ "Order-Finished "
      case _                   ⇒ ""
    }
}
