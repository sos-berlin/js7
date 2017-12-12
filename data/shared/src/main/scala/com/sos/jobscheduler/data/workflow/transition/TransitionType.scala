package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.data.order.{Order, Payload}
import com.sos.jobscheduler.data.workflow.WorkflowRoute
import com.sos.jobscheduler.data.workflow.transition.TransitionType._
import scala.collection.immutable.{IndexedSeq, Seq}

/**
  * @author Joacim Zschimmer
  */
trait TransitionType {

  type InputOrder = Order[Order.Transitionable]

  def routesMinimum: Int
  def routesMaximum: Option[Int]

  def result(orders: IndexedSeq[InputOrder], childRoutes: IndexedSeq[WorkflowRoute]): Result

  protected def singleOrder(orders: Seq[InputOrder]): InputOrder =
    numberedOrders(1, orders).head

  protected def numberedOrders(n: Int, orders: Seq[InputOrder]): IndexedSeq[InputOrder] = {
    if (orders.size != n) sys.error(s"Transition ${getClass.scalaName} requires $n input orders, but got ${orders.size}")
    if (!orders.forall(_.state == Order.Processed)) sys.error(s"Transition ${getClass.scalaName} processes only 'Processed' orders")
    orders.toVector
  }
}

object TransitionType {

  sealed trait Result

  final case class Move(to: Int) extends Result

  final case class Fork(children: Seq[Fork.Child]) extends Result
  object Fork {
    final case class Child(routeId: WorkflowRoute.Id, payload: Payload)
  }

  final case class Join(payload: Payload) extends Result
}
