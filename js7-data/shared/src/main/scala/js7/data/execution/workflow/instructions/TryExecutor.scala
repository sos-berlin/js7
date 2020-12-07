package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.base.utils.Assertions.assertThat
import js7.data.execution.workflow.context.OrderContext
import js7.data.order.Order
import js7.data.order.OrderEvent.OrderMoved
import js7.data.workflow.instructions.TryInstruction
import js7.data.workflow.position.BranchId.try_
import js7.data.workflow.position.Position

/**
  * @author Joacim Zschimmer
  */
object TryExecutor extends PositionInstructionExecutor with EventInstructionExecutor {

  type Instr = TryInstruction

  def nextPosition(instruction: TryInstruction, order: Order[Order.State], context: OrderContext): Checked[Option[Position]] = {
    assertThat(Right(order) == context.idToOrder(order.id).map(_ withPosition order.position))
    Right(Some(nextPos(order)))
  }

  def toEvents(instruction: TryInstruction, order: Order[Order.State], context: OrderContext) =
    Right(
      order.ifState[Order.IsFreshOrReady].map(order =>
        order.id <-: OrderMoved(nextPos(order)))
      .toList)

  private def nextPos(order: Order[Order.State]) =
    order.position / try_(0) % 0
}
