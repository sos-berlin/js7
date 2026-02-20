package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.data.order.Order
import js7.data.order.OrderEvent.OrderMoved
import js7.data.state.EngineState

trait PositionInstructionExecutor extends InstructionExecutor:

  def nextMove(
    instruction: Instr,
    order: Order[Order.State],
    engineState: EngineState,
    now: Timestamp)
  : Checked[Option[OrderMoved]]
