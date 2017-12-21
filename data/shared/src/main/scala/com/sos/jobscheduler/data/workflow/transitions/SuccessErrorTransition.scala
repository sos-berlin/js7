package com.sos.jobscheduler.data.workflow.transitions

import com.sos.jobscheduler.data.workflow.WorkflowGraph
import com.sos.jobscheduler.data.workflow.transition.SingleInputTransition
import com.sos.jobscheduler.data.workflow.transition.TransitionType.Move
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
case object SuccessErrorTransition extends SingleInputTransition {

  def graphsMaximum = Some(2)

  def result(order: InputOrder, graphIds: Seq[WorkflowGraph.Id]) =
    Move(if (order.outcome.isSuccess) 0 else 1)
}
