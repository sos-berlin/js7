package com.sos.jobscheduler.core.workflow.instructions

import cats.data.Validated.Valid
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.OrderMoved
import com.sos.jobscheduler.data.workflow.instructions.TryInstruction
import com.sos.jobscheduler.data.workflow.position.BranchId.Try_

/**
  * @author Joacim Zschimmer
  */
object TryExecutor extends PositionInstructionExecutor with EventInstructionExecutor {

  type Instr = TryInstruction

  def nextPosition(context: OrderContext, order: Order[Order.State], instruction: TryInstruction) = {
    assert(order == context.idToOrder(order.id).withPosition(order.position))
    Valid(Some(order.position / Try_ % 0))
  }

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: TryInstruction) =
    Valid(
      order.ifState[Order.FreshOrReady].map(order =>
        order.id <-: OrderMoved(order.position / Try_ % 0)))
}
