package com.sos.jobscheduler.master

import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.data.event.Stamped
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrderId, OrdersOverview}
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait OrderApi {

  def order(orderId: OrderId): Task[Option[Order[Order.State]]]

  def orders: Task[Stamped[Seq[Order[Order.State]]]]

  def orderIds = orders map (_ map (_ map (_.id)))

  def ordersOverview: Task[OrdersOverview] =
    for (c <- orderCount) yield
      OrdersOverview(count = c)

  def orderCount: Task[Int]
}

object OrderApi {
  trait WithCommands extends OrderApi {
    def addOrder(order: FreshOrder): Task[Checked[Boolean]]

    def addOrders(orders: Seq[FreshOrder]): Task[Checked[Completed]]
  }
}
