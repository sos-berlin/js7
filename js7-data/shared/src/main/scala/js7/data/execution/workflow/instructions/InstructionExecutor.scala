package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.context.StateView
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderActorEvent, OrderMoved}
import js7.data.workflow.Instruction
import js7.data.workflow.position.Position

/**
  * @author Joacim Zschimmer
  */
trait InstructionExecutor {
  type Instr <: Instruction
}

trait EventInstructionExecutor extends InstructionExecutor
{
  def toEvents(instruction: Instr, order: Order[Order.State], stateView: StateView)
  : Checked[List[KeyedEvent[OrderActorEvent]]]
}

trait PositionInstructionExecutor extends InstructionExecutor
{
  def nextPosition(instruction: Instr, order: Order[Order.State], stateView: StateView)
  : Checked[Option[Position]]
}

object InstructionExecutor
{
  private[instructions] def ifProcessedThenOrderMoved(order: Order[Order.State], stateView: StateView) =
    order.ifState[Order.Processed].map(order =>
      order.id <-: OrderMoved(order.position.increment))
}
