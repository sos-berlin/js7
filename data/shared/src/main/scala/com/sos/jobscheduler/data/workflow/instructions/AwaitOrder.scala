package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAwaiting, OrderJoined}
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.Instructions.ifProcessedThenOrderMoved
import com.sos.jobscheduler.data.workflow.{EventInstruction, OrderContext}

/**
  * @author Joacim Zschimmer
  */
final case class AwaitOrder(orderId: OrderId) extends EventInstruction
{
  def toEvent(order: Order[Order.State], context: OrderContext) =
    order.ifState[Order.Idle].map(
      _.id <-: OrderAwaiting(orderId))
    .orElse(
        for {
          order ← order.ifState[Order.Awaiting]
          _ ← context.idToOrder.lift(orderId) flatMap (_.ifState[Order.Offered])
        } yield
          order.id <-: OrderJoined(MapDiff.empty, Outcome.succeeded))
    .orElse(
      ifProcessedThenOrderMoved(order, context))

  override def toString = s"await orderId=$orderId"
}
