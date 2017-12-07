package com.sos.jobscheduler.data.workflow.transitions

import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.transition.SingleInputTransition
import com.sos.jobscheduler.data.workflow.transition.TransitionType.{Fork, Outlet}
import scala.collection.immutable.IndexedSeq

/**
  * @author Joacim Zschimmer
  */
case object ForkTransition extends SingleInputTransition {

  val outletsMaximum = None

  def result(order: InputOrder, outlets: IndexedSeq[Outlet]) =
    Fork(
      for (outlet ← outlets) yield
        Fork.Child(outlet, forkOrderId(order.id, outlet), order.payload))

  // TODO Sicherstellen, dass Schrägstrich in einer OrderId nur hier verwendet wird, damit sie eindeutig ist.
  private def forkOrderId(orderId: OrderId, outlet: Outlet) =
    OrderId(s"${orderId.string}/${outlet.id.string}")

  override def toString = "ForkTransition"
}
