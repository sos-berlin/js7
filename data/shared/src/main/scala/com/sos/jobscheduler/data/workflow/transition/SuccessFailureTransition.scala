package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.data.workflow.transition.TransitionType.{Move, Outlet}
import scala.collection.immutable.IndexedSeq

/**
  * @author Joacim Zschimmer
  */
case object SuccessFailureTransition extends SingleInputTransition {

  def outletsMaximum = Some(2)

  def result(order: InputOrder, outlets: IndexedSeq[Outlet]) =
    Move(if (order.outcome.isSuccess) outlets(0) else outlets(1))
}
