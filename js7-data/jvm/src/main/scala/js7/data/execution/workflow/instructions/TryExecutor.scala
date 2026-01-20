package js7.data.execution.workflow.instructions

import js7.data.order.Order
import js7.data.order.OrderEvent.OrderMoved
import js7.data.state.EngineState
import js7.data.workflow.instructions.TryInstruction
import js7.data.workflow.position.*
import js7.data.workflow.position.BranchId.try_
import js7.data.workflow.position.BranchPath.syntax.*

private[instructions] final class TryExecutor(protected val service: InstructionExecutorService)
extends PositionInstructionExecutor, EventInstructionExecutor:

  type Instr = TryInstruction
  val instructionClass = classOf[TryInstruction]

  def nextMove(instruction: TryInstruction, order: Order[Order.State], state: EngineState) =
    Right(Some(nextOrderMoved(order)))

  def toEvents(instruction: TryInstruction, order: Order[Order.State], engineState: EngineState) =
    Right:
      order.ifState[Order.IsFreshOrReady].map: order =>
        order.id <-: nextOrderMoved(order)
      .toList

  private def nextOrderMoved(order: Order[Order.State]) =
    OrderMoved(order.position / try_(0) % 0)

  override def subworkflowEndToPosition(parentPos: Position) =
    Some(parentPos.increment)
