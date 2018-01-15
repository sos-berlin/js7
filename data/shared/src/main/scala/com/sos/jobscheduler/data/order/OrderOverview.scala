package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.data.workflow.WorkflowPosition

/**
  * @author Joacim Zschimmer
  */
final case class OrderOverview(
  id: OrderId,
  workflowPosition: WorkflowPosition,
  state: Order.State)

object OrderOverview {
  def fromOrder(order: Order[Order.State]) = OrderOverview(order.id, order.workflowPosition, order.state)

  implicit val jsonCodec = deriveCirceCodec[OrderOverview]
}
