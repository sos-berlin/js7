package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.data.order.Order
import js7.data.order.OrderEvent.OrderMoved
import js7.data.state.StateView
import js7.data.workflow.instructions.TryInstruction
import js7.data.workflow.position.BranchId.try_
import js7.data.workflow.position.Position

private[instructions] final class TryExecutor(protected val service: InstructionExecutorService)
extends PositionInstructionExecutor with EventInstructionExecutor {

  type Instr = TryInstruction
  val instructionClass = classOf[TryInstruction]

  def nextPosition(instruction: TryInstruction, order: Order[Order.State], state: StateView): Checked[Option[Position]] =
    Right(Some(nextPos(order)))

  def toEvents(instruction: TryInstruction, order: Order[Order.State], stateView: StateView) =
    Right(
      order.ifState[Order.IsFreshOrReady].map(order =>
        order.id <-: OrderMoved(nextPos(order)))
      .toList)

  private def nextPos(order: Order[Order.State]) =
    order.position / try_(0) % 0
}
