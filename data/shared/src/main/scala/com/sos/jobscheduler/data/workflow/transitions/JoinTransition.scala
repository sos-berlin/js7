package com.sos.jobscheduler.data.workflow.transitions

import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.transition.TransitionType
import com.sos.jobscheduler.data.workflow.transition.TransitionType.Join
import org.scalactic.Requirements._
import scala.collection.immutable.{IndexedSeq, Seq}

/**
  * @author Joacim Zschimmer
  */
case object JoinTransition extends TransitionType {

  val graphsMinimum = 1
  val graphsMaximum = None

  def result(orders: IndexedSeq[InputOrder], childIds: Seq[OrderId.Child]) = {
    val forkedOrder = orders.head
    require(forkedOrder.state.isInstanceOf[Order.Forked])
    Join(forkedOrder.payload)
  }
}
