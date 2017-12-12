package com.sos.jobscheduler.master

import com.sos.jobscheduler.data.event.Stamped
import com.sos.jobscheduler.data.order.{Order, OrderId, OrderOverview, OrdersOverview}
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
trait OrderClient {

  protected implicit def executionContext: ExecutionContext

  def order(orderId: OrderId): Future[Option[Order[Order.State]]]

  def orders: Future[Stamped[Seq[Order[Order.State]]]]

  def orderOverviews: Future[Stamped[Seq[OrderOverview]]] =
    for (oo ← orders) yield
      oo map { _ map OrderOverview.fromOrder }

  def ordersOverview: Future[OrdersOverview] =
    for (c ← orderCount) yield
      OrdersOverview(orderCount = c)

  def orderCount: Future[Int]
}
