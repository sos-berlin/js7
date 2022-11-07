package js7.data.execution.workflow.instructions

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

  def nextMove(instruction: TryInstruction, order: Order[Order.State], state: StateView) =
    Right(Some(nextOrderMoved(order)))

  def toEvents(instruction: TryInstruction, order: Order[Order.State], stateView: StateView) =
    Right(
      order.ifState[Order.IsFreshOrReady].map(order =>
        order.id <-: nextOrderMoved(order))
      .toList)

  private def nextOrderMoved(order: Order[Order.State]) =
    OrderMoved(order.position / try_(0) % 0)

  override def subworkflowEndToPosition(parentPos: Position) =
    Some(parentPos.increment)
}
