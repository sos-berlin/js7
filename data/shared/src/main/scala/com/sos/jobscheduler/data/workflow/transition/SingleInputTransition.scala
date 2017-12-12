package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.data.workflow.WorkflowRoute
import scala.collection.immutable.IndexedSeq

/**
  * TransitionType from one node to another one.
  *
  * @author Joacim Zschimmer
  */
trait SingleInputTransition extends TransitionType {

  val routesMinimum = 1

  def result(order: InputOrder, routes: IndexedSeq[WorkflowRoute]): TransitionType.Result

  final def result(orders: IndexedSeq[InputOrder], routes: IndexedSeq[WorkflowRoute]) =
    result(singleOrder(orders), routes)
}
