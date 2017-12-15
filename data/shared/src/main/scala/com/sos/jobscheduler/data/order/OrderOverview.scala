package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.data.workflow.NodeKey

/**
  * @author Joacim Zschimmer
  */
final case class OrderOverview(
  id: OrderId,
  nodeKey: NodeKey,
  state: Order.State)

object OrderOverview {
  def fromOrder(order: Order[Order.State]) = OrderOverview(order.id, order.nodeKey, order.state)

  implicit val jsonCodec = deriveCirceCodec[OrderOverview]
}
