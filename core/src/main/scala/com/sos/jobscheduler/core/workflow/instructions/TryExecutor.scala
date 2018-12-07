package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderMoved}
import com.sos.jobscheduler.data.workflow.OrderContext
import com.sos.jobscheduler.data.workflow.instructions.TryInstruction
import com.sos.jobscheduler.data.workflow.position.Position

/**
  * @author Joacim Zschimmer
  */
object TryExecutor extends PositionInstructionExecutor with EventInstructionExecutor {

  type Instr = TryInstruction

  def nextPosition(context: OrderContext, order: Order[Order.State], instruction: TryInstruction): Option[Position] = {
    assert(order == context.idToOrder(order.id).withPosition(order.position))
    Some(firstTryPosition(order))
  }

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: TryInstruction): Option[KeyedEvent[OrderActorEvent]] =
    order.ifState[Order.FreshOrReady].map(order ⇒
      order.id <-: OrderMoved(firstTryPosition(order)))

  private def firstTryPosition(order: Order[Order.State]) = order.position / TryInstruction.TryBranchId / 0
}
