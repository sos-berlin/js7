package com.sos.jobscheduler.data.workflow.transitions

import com.sos.jobscheduler.data.workflow.WorkflowGraph
import com.sos.jobscheduler.data.workflow.transition.SingleInputTransition
import com.sos.jobscheduler.data.workflow.transition.TransitionType.Fork
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
case object ForkTransition extends SingleInputTransition {

  val graphsMaximum = None

  def result(order: InputOrder, graphIds: Seq[WorkflowGraph.Id]) =
    Fork(
      for (id ← graphIds) yield
        Fork.Child(id, order.payload))


  override def toString = "ForkTransition"
}
