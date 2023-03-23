package js7.data.execution.workflow.instructions

import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.execution.workflow.OrderEventSource.leaveBlocks
import js7.data.order.Order
import js7.data.order.OrderEvent.OrderCycleFinished
import js7.data.state.StateView
import js7.data.workflow.instructions.Break
import js7.data.workflow.position.BranchId

private[instructions] final class BreakExecutor(
  protected val service: InstructionExecutorService)
extends EventInstructionExecutor
{
  type Instr = Break
  val instructionClass = classOf[Break]

  def toEvents(instr: Break, order: Order[Order.State], state: StateView) =
    for {
      workflow <- state.idToWorkflow.checked(order.workflowId)
      events <- leaveBlocks(
        workflow, order,
        until = _.string startsWith BranchId.CyclePrefix,
        events = OrderCycleFinished(None) :: Nil)
    } yield
      events.map(order.id <-: _)
}
