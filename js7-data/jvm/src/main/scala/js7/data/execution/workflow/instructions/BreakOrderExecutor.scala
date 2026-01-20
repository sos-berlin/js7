package js7.data.execution.workflow.instructions

import js7.data.order.Order
import js7.data.order.OrderEvent.OrderBroken
import js7.data.state.EngineState
import js7.data.workflow.instructions.BreakOrder

private[instructions] final class BreakOrderExecutor(
  protected val service: InstructionExecutorService)
extends EventInstructionExecutor:

  type Instr = BreakOrder
  val instructionClass = classOf[BreakOrder]

  def toEvents(instr: BreakOrder, order: Order[Order.State], state: EngineState) =
    Right((order.id <-: OrderBroken()) :: Nil)
