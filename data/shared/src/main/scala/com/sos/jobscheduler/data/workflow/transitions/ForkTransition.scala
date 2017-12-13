package com.sos.jobscheduler.data.workflow.transitions

import com.sos.jobscheduler.data.workflow.WorkflowRoute
import com.sos.jobscheduler.data.workflow.transition.SingleInputTransition
import com.sos.jobscheduler.data.workflow.transition.TransitionType.Fork
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
case object ForkTransition extends SingleInputTransition {

  val routesMaximum = None

  def result(order: InputOrder, routeIds: Seq[WorkflowRoute.Id]) =
    Fork(
      for (id ← routeIds) yield
        Fork.Child(id, order.payload))


  override def toString = "ForkTransition"
}
