package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.data.order.LeanOrder
import scala.collection.immutable.Seq

/**
  * TransitionType from one node to another one.
  *
  * @author Joacim Zschimmer
  */
trait OneToOneTransition extends TransitionType {

  def result(order: LeanOrder): Int

  final def results(orders: Seq[LeanOrder]) =
    result(singleOrder(orders))
}
