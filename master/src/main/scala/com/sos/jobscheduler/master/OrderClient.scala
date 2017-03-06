package com.sos.jobscheduler.master

import com.sos.jobscheduler.data.order.{Order, OrderId}
import scala.collection.immutable.Seq
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
trait OrderClient {

  def order(orderId: OrderId): Future[Option[Order[Order.State]]]

  def orders: Future[Seq[Order[Order.State]]]
}
