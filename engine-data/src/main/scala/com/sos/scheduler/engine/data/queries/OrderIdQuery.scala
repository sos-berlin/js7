package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.order.OrderId
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class OrderIdQuery(orderId: Option[OrderId] = None) {

  def matchesOrder(o: QueryableOrder) = orderId forall { _ == o.orderKey.id }
}

object OrderIdQuery {
  val All = OrderIdQuery()
  implicit val jsonFormat = jsonFormat1(apply)
}
