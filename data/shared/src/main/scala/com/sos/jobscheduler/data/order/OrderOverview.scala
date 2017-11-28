package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.data.workflow.NodeKey
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class OrderOverview(
  id: OrderId,
  nodeKey: NodeKey,
  state: Order.State)

object OrderOverview {
  def fromOrder(order: Order[Order.State]) = OrderOverview(order.id, order.nodeKey, order.state)
}
