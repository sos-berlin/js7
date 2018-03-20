package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderDetachable}
import com.sos.jobscheduler.data.workflow.OrderContext
import com.sos.jobscheduler.data.workflow.instructions.Gap

/**
  * @author Joacim Zschimmer
  */
object GapExecutor extends EventInstructionExecutor {

  type Instr = Gap

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: Gap): Option[KeyedEvent[OrderActorEvent]] =
    Some(order.id <-: OrderDetachable)
}
