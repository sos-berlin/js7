package com.sos.jobscheduler.data.workflow.transitions

import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.workflow.transition.TransitionType
import com.sos.jobscheduler.data.workflow.transition.TransitionType.{Join, Outlet}
import org.scalactic.Requirements._
import scala.collection.immutable.IndexedSeq

/**
  * @author Joacim Zschimmer
  */
case object JoinTransition extends TransitionType {

  val outletsMinimum = 1
  val outletsMaximum = None

  def result(orders: IndexedSeq[JoinTransition.InputOrder], outlets: IndexedSeq[Outlet]) = {
    val forkedOrder = orders.head
    require(forkedOrder.state.isInstanceOf[Order.Forked])
    Join(forkedOrder.payload)
  }
}
