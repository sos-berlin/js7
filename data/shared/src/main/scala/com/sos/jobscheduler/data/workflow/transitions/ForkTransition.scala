package com.sos.jobscheduler.data.workflow.transitions

import com.sos.jobscheduler.data.workflow.WorkflowRoute
import com.sos.jobscheduler.data.workflow.transition.SingleInputTransition
import com.sos.jobscheduler.data.workflow.transition.TransitionType.Fork
import scala.collection.immutable.IndexedSeq

/**
  * @author Joacim Zschimmer
  */
case object ForkTransition extends SingleInputTransition {

  val routesMaximum = None

  def result(order: InputOrder, routes: IndexedSeq[WorkflowRoute]) =
    Fork(
      for (route ← routes) yield
        Fork.Child(route.id, order.payload))


  override def toString = "ForkTransition"
}
