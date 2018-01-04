package com.sos.jobscheduler.data.workflow.transitions

import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.transition.SingleInputTransition
import com.sos.jobscheduler.data.workflow.transition.TransitionType.Fork
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
case object ForkTransition extends SingleInputTransition {

  val graphsMaximum = None

  def result(order: InputOrder, childIds: Seq[OrderId.Child]) =
    Fork(
      for (id ← childIds) yield
        Fork.Child(id, order.payload))


  override def toString = "ForkTransition"
}
