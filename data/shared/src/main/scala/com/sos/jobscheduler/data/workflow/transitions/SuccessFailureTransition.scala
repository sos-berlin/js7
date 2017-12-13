package com.sos.jobscheduler.data.workflow.transitions

import com.sos.jobscheduler.data.workflow.WorkflowRoute
import com.sos.jobscheduler.data.workflow.transition.SingleInputTransition
import com.sos.jobscheduler.data.workflow.transition.TransitionType.Move
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
case object SuccessFailureTransition extends SingleInputTransition {

  def routesMaximum = Some(2)

  def result(order: InputOrder, routeIds: Seq[WorkflowRoute.Id]) =
    Move(if (order.outcome.isSuccess) 0 else 1)
}
