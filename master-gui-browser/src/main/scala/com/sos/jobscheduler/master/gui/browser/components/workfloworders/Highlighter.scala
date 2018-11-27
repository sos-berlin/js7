package com.sos.jobscheduler.master.gui.browser.components.workfloworders

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.master.gui.browser.components.workfloworders.Highlighter._
import japgolly.scalajs.react.Callback
import org.scalajs.dom.html
import scala.collection.mutable
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
private class Highlighter {
  private var _lastHighlightedAt = Timestamp.currentTimeMillis
  def lastHighlightedAt = _lastHighlightedAt
  private val orderToHighlighted, orderToCleanup = new mutable.HashMap[OrderId, HighlightedElement]

  def onChanged(orderId: OrderId, row: html.Element): Unit = {
    orderToHighlighted += orderId → HighlightedElement(row, Timestamp.currentTimeMillis)
    orderToCleanup -= orderId
  }

  def componentDidUpdate(): Unit = {
    _lastHighlightedAt = Timestamp.currentTimeMillis
    Callback { phaseout() }.delayMs(HighlightMillis + 100).runNow()
  }

  private def phaseout(): Unit = {
    val now = Timestamp.currentTimeMillis
    val phasingOut = orderToHighlighted.filter(_._2.startPhaseoutAt < now)
    for (h ← phasingOut.values) {
      val o = h.row.classList
      o.remove("Boxed-Order-changed")
      o.add("Boxed-Order-changed-phaseout")
    }
    orderToHighlighted --= phasingOut.keys
    orderToCleanup ++= phasingOut
    Callback { cleanUp() }.delayMs(HighlightPhaseoutMillis + 100).runNow()
  }

  private def cleanUp(): Unit = {
    val now = Timestamp.currentTimeMillis
    val cleansing = orderToCleanup filter (_._2.endPhaseoutAt < now)
    for (h ← cleansing.values) {
      h.row.classList.remove("Boxed-Order-changed-phaseout")
    }
    orderToCleanup --= cleansing.keys
  }

  def abortPhasingOut(orderId: OrderId): Callback =
    Callback {
      for (h ← orderToCleanup.get(orderId)) {
        h.row.classList.remove("Boxed-Order-changed-phaseout")
      }
    }
}

object Highlighter {
  private val HighlightMillis = 5.seconds.toMillis
  private val HighlightPhaseoutMillis = 1.seconds.toMillis

  private case class HighlightedElement(row: html.Element, start: Long) {
    def startPhaseoutAt = start + HighlightMillis
    def endPhaseoutAt = startPhaseoutAt + HighlightPhaseoutMillis
  }
}
