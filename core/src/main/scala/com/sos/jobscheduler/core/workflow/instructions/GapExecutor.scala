package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderDetachable}
import com.sos.jobscheduler.data.workflow.instructions.Gap

/**
  * @author Joacim Zschimmer
  */
object GapExecutor extends EventInstructionExecutor {

  type Instr = Gap

  val logger = Logger(getClass)

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: Gap): Option[KeyedEvent[OrderActorEvent]] =
    if (order.isAttached)
      Some(order.id <-: OrderDetachable)
    else {
      logger.error(s"Instruction Gap but order is not attached to an agent: $order")
      None
    }
}
