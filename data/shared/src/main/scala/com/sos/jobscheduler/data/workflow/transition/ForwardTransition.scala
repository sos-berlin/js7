package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.data.workflow.WorkflowGraph
import com.sos.jobscheduler.data.workflow.transition.TransitionType.Move
import scala.collection.immutable.Seq

case object ForwardTransition extends SingleInputTransition {

  val graphsMaximum = Some(1)

  def result(order: InputOrder, graphIds: Seq[WorkflowGraph.Id]) =
    Move(0)
}
