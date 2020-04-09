package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.OrderMoved
import com.sos.jobscheduler.data.workflow.instructions.TryInstruction
import com.sos.jobscheduler.data.workflow.position.BranchId.try_

/**
  * @author Joacim Zschimmer
  */
object TryExecutor extends PositionInstructionExecutor with EventInstructionExecutor {

  type Instr = TryInstruction

  def nextPosition(context: OrderContext, order: Order[Order.State], instruction: TryInstruction) = {
    assert(order == context.idToOrder(order.id).withPosition(order.position))
    Right(Some(nextPos(order)))
  }

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: TryInstruction) =
    Right(
      order.ifState[Order.IsFreshOrReady].map(order =>
        order.id <-: OrderMoved(nextPos(order))))

  private def nextPos(order: Order[Order.State]) =
    order.position / try_(0) % 0
}
