package com.sos.jobscheduler.data.workflow.transitions

import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.workflow.WorkflowGraph
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

  def result(orders: IndexedSeq[InputOrder], graphIds: Seq[WorkflowGraph.Id]) = {
    val forkedOrder = orders.head
    require(forkedOrder.state.isInstanceOf[Order.Forked])
    Join(forkedOrder.payload)
  }
}
