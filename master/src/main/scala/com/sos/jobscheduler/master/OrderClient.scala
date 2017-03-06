package com.sos.jobscheduler.master

import com.sos.jobscheduler.data.order.{Order, OrderId, OrderOverview}
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
trait OrderClient {

  protected implicit def executionContext: ExecutionContext

  def order(orderId: OrderId): Future[Option[Order[Order.State]]]

  def orders: Future[Seq[Order[Order.State]]]

  def orderOverviews: Future[Seq[OrderOverview]] =
    for (oo ← orders) yield
      oo map OrderOverview.fromOrder
}
