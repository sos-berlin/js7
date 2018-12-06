package com.sos.jobscheduler.master.gui.browser.components.workfloworders

import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.master.gui.browser.common.Renderers.forTable.orderStateToVdom
import com.sos.jobscheduler.master.gui.browser.common.Renderers.orderStateToSymbol
import com.sos.jobscheduler.master.gui.browser.components.state.OrdersState
import com.sos.jobscheduler.master.gui.browser.components.state.OrdersState.{Mark, OrderEntry}
import com.sos.jobscheduler.master.gui.browser.router.Router
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import org.scalajs.dom.window

/**
  * @author Joacim Zschimmer
  */
private[workfloworders] final class BoxedOrderComponent(
  orderOnMouseOver: OrderId ⇒ Callback,
  orderOnMouseOut: OrderId ⇒ Callback,
  orderOnClick: OrderId ⇒ Callback)
{
  def apply(orderEntry: OrderEntry) = component(orderEntry)

  private val component = ScalaComponent.builder[OrdersState.OrderEntry]("Little-Order")
    .render_P {
      case OrdersState.OrderEntry(order, _, lastOutput, mark, updatedAt) ⇒
        <.div(^.cls := "sheet depth-1 Boxed-Order " + orderToClass(order) + markToClass(mark),
          ^.title := "Double-click for details",
          ^.onMouseOver --> orderOnMouseOver(order.id),
          ^.onMouseOut --> orderOnMouseOut(order.id),
          ^.onClick --> orderOnClick(order.id),
          ^.onDoubleClick --> Callback { window.document.location.assign(Router.hash(order.id)) },
          <.div(^.cls := "Boxed-Order-OrderId",
            order.id.string),
          order.state match {
            case _: Order.Fresh | _: Order.Forked ⇒
              <.div(^.cls := "Boxed-Order-compact",
                orderStateToVdom(order))

            case Order.Processing ⇒
              <.div(^.cls := "Boxed-Order-compact",
                orderStateToSymbol(order), " ",
                lastOutput getOrElse order.state.toString: String)

            case _ ⇒
              <.div(
                <.span(^.cls := "Boxed-Order-State", orderStateToVdom(order)))
          },
          <.div(^.cls := "Boxed-Order-cancel", "CANCELED") when order.cancel.isDefined)
    }
    .configure {
      implicit val orderEntryReuse = Reusability.byRef[OrdersState.OrderEntry]
      Reusability.shouldComponentUpdate
    }
    .build

  private def orderToClass(order: Order[Order.State]): String =
    order.state match {
      case _: Order.Fresh   ⇒ "Order-Fresh "
      case Order.Processing ⇒ "Order-Processing "
      case _: Order.Forked  ⇒ "Order-Forked "
      case Order.Finished   ⇒ "Order-Finished "
      case _: Order.Stopped ⇒ "Order-Stopped "
      case _: Order.Broken  ⇒ "Order-Broken "
      case _                ⇒ ""
    }

  private def markToClass(mark: Option[Mark]) =
    mark match {
      case None ⇒ ""
      case Some(Mark(false, false)) ⇒ "Boxed-Order-marked-permanently "
      case Some(Mark(false, true )) ⇒ "Boxed-Order-relative-marked-permanently "
      case Some(Mark(true , false)) ⇒ "Boxed-Order-marked-volatile "
      case Some(Mark(true , true )) ⇒ "Boxed-Order-relative-marked-volatile "
    }
}
