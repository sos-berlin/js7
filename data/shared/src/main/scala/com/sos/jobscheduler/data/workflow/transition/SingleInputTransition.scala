package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.data.order.OrderId
import scala.collection.immutable.{IndexedSeq, Seq}

/**
  * TransitionType from one node to another one.
  *
  * @author Joacim Zschimmer
  */
trait SingleInputTransition extends TransitionType {

  val graphsMinimum = 1

  def result(order: InputOrder, orderChildIds: Seq[OrderId.Child]): TransitionType.Result

  final def result(orders: IndexedSeq[InputOrder], childIds: Seq[OrderId.Child]) =
    result(singleOrder(orders), childIds)
}
