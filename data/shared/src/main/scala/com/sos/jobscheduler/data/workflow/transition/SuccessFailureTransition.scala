package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.data.order.LeanOrder
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
case object SuccessFailureTransition extends TransitionType {

  def results(orders: Seq[LeanOrder]) = {
    val outcome = singleOrder(orders).outcome
    if (outcome.isSuccess) 0 else 1
  }
}
