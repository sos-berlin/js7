package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.data.workflow.WorkflowRoute
import com.sos.jobscheduler.data.workflow.transition.TransitionType.Move
import scala.collection.immutable.IndexedSeq

/**
  * @author Joacim Zschimmer
  */
case object SuccessFailureTransition extends SingleInputTransition {

  def routesMaximum = Some(2)

  def result(order: InputOrder, routes: IndexedSeq[WorkflowRoute]) =
    Move(if (order.outcome.isSuccess) 0 else 1)
}
