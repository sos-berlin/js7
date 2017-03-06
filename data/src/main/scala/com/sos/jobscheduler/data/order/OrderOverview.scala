package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.data.jobnet.NodeKey
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class OrderOverview(
  id: OrderId,
  nodeKey: NodeKey,
  state: Order.State)

object OrderOverview {
  implicit val jsonFormat = jsonFormat3(apply)

  def fromOrder(order: Order[Order.State]) = OrderOverview(order.id, order.nodeKey, order.state)
}
