package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.OrderDetachable
import com.sos.jobscheduler.data.workflow.{EventInstruction, OrderContext}

/**
  * @author Joacim Zschimmer
  */
sealed trait Gap extends EventInstruction
/** reduceForAgent uses Gap for all instructions not executable on the requested Agent. */

case object Gap extends Gap
{
  def toEvent(order: Order[Order.State], context: OrderContext) =
    Some(order.id <-: OrderDetachable)

  override def toString = "gap"
}
