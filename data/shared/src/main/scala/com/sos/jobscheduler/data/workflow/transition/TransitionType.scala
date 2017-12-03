package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.data.order.LeanOrder
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
trait TransitionType {

  def results(orders: Seq[LeanOrder]): Int

  protected def singleOrder(orders: Seq[LeanOrder]): LeanOrder =
    numberedOrders(1, orders).head

  protected def numberedOrders(n: Int, orders: Seq[LeanOrder]): IndexedSeq[LeanOrder] = {
    if (orders.size != n) sys.error(s"Transition ${getClass.scalaName} requires $n input orders, but got ${orders.size}")
    orders.toVector
  }
}
