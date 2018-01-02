package com.sos.jobscheduler.master.gui.components.orderlist

import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.master.gui.components.orderlist.OrderSelector._
import japgolly.scalajs.react.{BackendScope, Callback}
import org.scalajs.dom.{MouseEvent, window}
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
private[orderlist] final class OrderSelector(scope: BackendScope[OrderListComponent.Props, Unit], highlighter: Highlighter)
{
  private var mouseOverOrderRow: Option[Selection] = None
  private var selectedOrderRow: Option[Selection] = None
  //private var selectedAt = 0L
  private var mouseMoved = false

  def onMouseMoved(event: MouseEvent): Callback =
    Callback {
      if (!mouseMoved) {
        mouseMoved = true
        selectedOrderRow foreach unhighlight
        val selectedOrderId = mouseOverOrderRow match {
          case Some(selection: Selection) ⇒
            highlight(selection)
            Some(selection.orderId)

          case None ⇒
            selectedOrderRow = None
            None
        }
        (scope.props >>= (_.ordersStateSnapshot.modState(_.copy(selectedOrder = selectedOrderId)))).runNow()
      }
    }

  private def unhighlight(selection: Selection): Unit = {
    for (e ← Option(window.document.getElementById(elementId(selection.orderId))))
      e.classList.remove(LinkHighlightClass)
    for (child ← selection.children;
         element ← Option(window.document.getElementById(elementId(child)))) {
      element.classList.remove(ChildOrderHighlightClass)
    }
  }

  private def highlight(selection: Selection): Unit = {
    for (o ← selection.children + selection.orderId) highlighter.abortPhasingOut(o).runNow() // clean for :hover
    for (element ← Option(window.document.getElementById(elementId(selection.orderId)))) {
      element.classList.add(LinkHighlightClass)
      selectedOrderRow = Some(selection)
    }
    for (child ← selection.children;
         element ← Option(window.document.getElementById(elementId(child)))) {
      element.classList.add(ChildOrderHighlightClass)
    }
  }

  def onMouseOverRow(order: Order[Order.State]): Unit = {
    mouseOverOrderRow = Some(Selection(order.id, orderChildren(order)))
    mouseMoved = false
  }

  def onMouseOutRow(orderId: OrderId): Unit = {
    mouseOverOrderRow = None
    mouseMoved = false
  }

  def updateOrder(order: Order[Order.State]): Unit =
    for (selection ← selectedOrderRow if selection.orderId == order.id) {
      val children = orderChildren(order)
      if (children != selection.children) {
        unhighlight(selection)
        val updated = Selection(order.id, children)
        highlight(updated)
        mouseOverOrderRow = Some(updated)
      }
    }

  def cssClass(orderId: OrderId) =
    selectedOrderRow match {
      case Some(Selection(`orderId`, _)) ⇒ LinkHighlightClass
      case Some(sel) if sel.children contains orderId ⇒ ChildOrderHighlightClass
      case _ ⇒ ""
    }

  //private def isTableChangeDelayed = Timestamp.epochMilli < selectedAt + DelayTableChangeDuration
}

object OrderSelector {
  private val LinkHighlightClass = "link-highlight"
  private val ChildOrderHighlightClass = "orders-child-highlight"
  private val DelayTableChangeDuration = 1.second.toMillis

  def elementId(orderId: OrderId) = orderId.toString  // With prefix "Order:"

  private def orderChildren(order: Order[Order.State]): Set[OrderId] =
    order.ifState[Order.Forked] match {
      case Some(o) ⇒ o.state.childOrderIds.toSet
      case _ ⇒ Set.empty
    }

  private case class Selection(orderId: OrderId, children: Set[OrderId])
}
