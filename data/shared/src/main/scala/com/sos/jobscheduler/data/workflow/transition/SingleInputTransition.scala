package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.data.workflow.WorkflowGraph
import scala.collection.immutable.{IndexedSeq, Seq}

/**
  * TransitionType from one node to another one.
  *
  * @author Joacim Zschimmer
  */
trait SingleInputTransition extends TransitionType {

  val graphsMinimum = 1

  def result(order: InputOrder, graphIds: Seq[WorkflowGraph.Id]): TransitionType.Result

  final def result(orders: IndexedSeq[InputOrder], graphIds: Seq[WorkflowGraph.Id]) =
    result(singleOrder(orders), graphIds)
}
