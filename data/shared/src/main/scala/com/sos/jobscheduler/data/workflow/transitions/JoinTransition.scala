package com.sos.jobscheduler.data.workflow.transitions

import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.workflow.WorkflowRoute
import com.sos.jobscheduler.data.workflow.transition.TransitionType
import com.sos.jobscheduler.data.workflow.transition.TransitionType.Join
import org.scalactic.Requirements._
import scala.collection.immutable.IndexedSeq

/**
  * @author Joacim Zschimmer
  */
case object JoinTransition extends TransitionType {

  val routesMinimum = 1
  val routesMaximum = None

  def result(orders: IndexedSeq[InputOrder], routes: IndexedSeq[WorkflowRoute]) = {
    val forkedOrder = orders.head
    require(forkedOrder.state.isInstanceOf[Order.Forked])
    Join(forkedOrder.payload)
  }
}
