package com.sos.jobscheduler.master

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.data.event.Stamped
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrderId, OrderOverview, OrdersOverview}
import monix.eval.Task
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
trait OrderApi {

  def order(orderId: OrderId): Task[Option[Order[Order.State]]]

  def orders: Task[Stamped[Seq[Order[Order.State]]]]

  def orderOverviews: Task[Stamped[Seq[OrderOverview]]] =
    for (oo ← orders) yield
      oo map { _ map OrderOverview.fromOrder }

  def ordersOverview: Task[OrdersOverview] =
    for (c ← orderCount) yield
      OrdersOverview(orderCount = c)

  def orderCount: Task[Int]
}

object OrderApi {
  trait WithCommands extends OrderApi {
    def addOrder(order: FreshOrder): Task[Checked[Boolean]]
  }
}
