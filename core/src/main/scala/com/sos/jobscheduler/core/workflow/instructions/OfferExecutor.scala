package com.sos.jobscheduler.core.workflow.instructions

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.core.workflow.instructions.InstructionExecutor.ifProcessedThenOrderMoved
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.{OrderOffered, OrderStarted}
import com.sos.jobscheduler.data.workflow.instructions.Offer

/**
  * @author Joacim Zschimmer
  */
object OfferExecutor extends EventInstructionExecutor
{
  type Instr = Offer

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: Offer) =
    Valid(
      order.ifState[Order.Fresh].map(order =>
        order.id <-: OrderStarted)
      .orElse(
        order.ifState[Order.Ready].map(
          _.id <-: OrderOffered(instruction.orderId, Timestamp.now + instruction.timeout))
        .orElse(
          ifProcessedThenOrderMoved(order, context))))
}
