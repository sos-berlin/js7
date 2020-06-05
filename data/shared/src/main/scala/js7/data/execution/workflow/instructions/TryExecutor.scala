package js7.data.execution.workflow.instructions

import js7.base.utils.Assertions.assertThat
import js7.data.execution.workflow.context.OrderContext
import js7.data.order.Order
import js7.data.order.OrderEvent.OrderMoved
import js7.data.workflow.instructions.TryInstruction
import js7.data.workflow.position.BranchId.try_

/**
  * @author Joacim Zschimmer
  */
object TryExecutor extends PositionInstructionExecutor with EventInstructionExecutor {

  type Instr = TryInstruction

  def nextPosition(context: OrderContext, order: Order[Order.State], instruction: TryInstruction) = {
    assertThat(order == context.idToOrder(order.id).withPosition(order.position))
    Right(Some(nextPos(order)))
  }

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: TryInstruction) =
    Right(
      order.ifState[Order.IsFreshOrReady].map(order =>
        order.id <-: OrderMoved(nextPos(order))))

  private def nextPos(order: Order[Order.State]) =
    order.position / try_(0) % 0
}
