package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.data.workflow.WorkflowRoute
import com.sos.jobscheduler.data.workflow.transition.TransitionType.Move
import scala.collection.immutable.IndexedSeq

case object ForwardTransition extends SingleInputTransition {

  val routesMaximum = Some(1)

  def result(order: InputOrder, routes: IndexedSeq[WorkflowRoute]) =
    Move(0)
}
