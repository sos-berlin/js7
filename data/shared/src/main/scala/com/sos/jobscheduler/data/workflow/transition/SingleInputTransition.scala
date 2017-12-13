package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.data.workflow.WorkflowRoute
import scala.collection.immutable.{IndexedSeq, Seq}

/**
  * TransitionType from one node to another one.
  *
  * @author Joacim Zschimmer
  */
trait SingleInputTransition extends TransitionType {

  val routesMinimum = 1

  def result(order: InputOrder, routeIds: Seq[WorkflowRoute.Id]): TransitionType.Result

  final def result(orders: IndexedSeq[InputOrder], routeIds: Seq[WorkflowRoute.Id]) =
    result(singleOrder(orders), routeIds)
}
