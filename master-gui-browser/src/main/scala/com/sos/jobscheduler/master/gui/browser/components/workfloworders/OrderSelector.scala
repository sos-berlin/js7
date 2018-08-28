package com.sos.jobscheduler.master.gui.browser.components.workfloworders

import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.master.gui.browser.components.workfloworders.OrderSelector._
import org.scalajs.dom.window

/**
  * @author Joacim Zschimmer
  */
private[workfloworders] final class OrderSelector
{
  private var mouseOverSelection: Option[Selection] = None
  private var clickedSelection: Option[Selection] = None

  def onMouseOver(order: Order[Order.State]): Unit = {
    if (!clickedSelection.exists(_.orderId == order.id)) {
      mouseOverSelection foreach unhighlight
      val selection = selectOrder(order)
      mouseOverSelection = Some(selection)
    }
  }

  def onMouseOut(orderId: OrderId): Unit = {
    if (!clickedSelection.exists(_.orderId == orderId)) {
      mouseOverSelection foreach unhighlight
      mouseOverSelection = None
    }
  }

  def onClick(order: Order[Order.State]): Unit = {
    if (mouseOverSelection.exists(_.orderId == order.id)) {
      mouseOverSelection = None
    }
    clickedSelection match {
      case Some(sel) if sel.orderId == order.id ⇒
        unhighlight(sel)
        clickedSelection = None
      case _ ⇒
        clickedSelection foreach unhighlight
        val selection = selectOrder(order)
        clickedSelection = Some(selection)
    }
  }

  private def selectOrder(order: Order[Order.State]): Selection = {
    val selection = Selection(order.id, orderChildren(order))
    highlight(selection)
    selection
  }

  private def unhighlight(selection: Selection): Unit = {
    for (e ← Option(window.document.getElementById(elementId(selection.orderId))))
      e.classList.remove(HighlightClass)
    for (child ← selection.children;
         element ← Option(window.document.getElementById(elementId(child)))) {
      element.classList.remove(ChildOrderHighlightClass)
    }
  }

  private def highlight(selection: Selection): Unit = {
    //for (o ← selection.children + selection.orderId) highlighter.abortPhasingOut(o).runNow() // clean for :hover
    for (element ← Option(window.document.getElementById(elementId(selection.orderId)))) {
      element.classList.add(HighlightClass)
    }
    for (child ← selection.children;
         element ← Option(window.document.getElementById(elementId(child)))) {
      element.classList.add(ChildOrderHighlightClass)
    }
  }

  def updateOrder(order: Order[Order.State]): Unit = {
    for (o ← mouseOverSelection) {
      updateSelection(order, o, sel ⇒ mouseOverSelection = Some(sel))
    }
    for (o ← clickedSelection) {
      updateSelection(order, o, sel ⇒ clickedSelection = Some(sel))
    }
  }

  private def updateSelection(order: Order[Order.State], selection: Selection, change: Selection ⇒ Unit): Unit = {
    lazy val children = orderChildren(order)
    if (selection.orderId == order.id && children != selection.children) {
      unhighlight(selection)
      val updated = Selection(order.id, children)
      highlight(updated)
      change(updated)
    }
  }

  def cssClass(orderId: OrderId): String =
    ((clickedSelection.iterator ++ mouseOverSelection.iterator).flatMap(_.cssClass(orderId)) ++ Iterator("")).next()
}

object OrderSelector {
  private val HighlightClass = "Order-highlight"
  private val ChildOrderHighlightClass = "Order-child-highlight"

  def elementId(orderId: OrderId) = orderId.toString  // With prefix "Order:"

  private def orderChildren(order: Order[Order.State]): Set[OrderId] =
    order.ifState[Order.Forked] match {
      case Some(o) ⇒ o.state.childOrderIds.toSet
      case _ ⇒ Set.empty
    }

  private case class Selection(orderId: OrderId, children: Set[OrderId]) {
    def cssClass(orderId: OrderId): Option[String] =
      if (orderId == this.orderId)
        Some(HighlightClass)
      else if (children contains orderId)
        Some(ChildOrderHighlightClass)
      else
        None
  }
}
