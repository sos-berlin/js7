package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.core.workflow.instructions.InstructionExecutor.ifProcessedThenOrderMoved
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderAwaiting, OrderJoined, OrderStarted}
import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.AwaitOrder

/**
  * @author Joacim Zschimmer
  */
object AwaitOrderExecutor extends EventInstructionExecutor
{
  type Instr = AwaitOrder

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: AwaitOrder): Option[KeyedEvent[OrderActorEvent]] =
    order.ifState[Order.Fresh].map(order ⇒
      order.id <-: OrderStarted)
    .orElse(
      order.ifState[Order.Ready].map(
        _.id <-: OrderAwaiting(instruction.orderId)))
    .orElse(
        for {
          order ← order.ifState[Order.Awaiting]
          _ ← context.idToOrder.lift(instruction.orderId) flatMap (_.ifState[Order.Offering])
        } yield
          order.id <-: OrderJoined(MapDiff.empty, Outcome.succeeded))
    .orElse(
      ifProcessedThenOrderMoved(order, context))
}
