package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.data.workflow.WorkflowRoute
import com.sos.jobscheduler.data.workflow.transition.TransitionType.Move
import scala.collection.immutable.Seq

case object ForwardTransition extends SingleInputTransition {

  val routesMaximum = Some(1)

  def result(order: InputOrder, routeIds: Seq[WorkflowRoute.Id]) =
    Move(0)
}
