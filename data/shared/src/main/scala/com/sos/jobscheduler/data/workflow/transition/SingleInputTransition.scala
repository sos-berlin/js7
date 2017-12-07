package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.data.workflow.transition.TransitionType.Outlet
import scala.collection.immutable.IndexedSeq

/**
  * TransitionType from one node to another one.
  *
  * @author Joacim Zschimmer
  */
trait SingleInputTransition extends TransitionType {

  val outletsMinimum = 1

  def result(order: InputOrder, outlets: IndexedSeq[Outlet]): TransitionType.Result

  final def result(orders: IndexedSeq[InputOrder], outlets: IndexedSeq[Outlet]) = {
    result(singleOrder(orders), outlets)
  }
}
