package js7.data.execution.workflow.instructions

import js7.base.problem.Problem
import js7.data.order.Order
import js7.data.order.OrderEvent.OrderDetachable
import js7.data.state.StateView
import js7.data.workflow.instructions.Gap

private[instructions] final class GapExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor {

  type Instr = Gap
  val instructionClass = classOf[Gap]

  def toEvents(instruction: Gap, order: Order[Order.State], state: StateView) =
    if (!order.isAttached)
      Left(Problem.pure(s"Instruction Gap but order is not attached to an agent: $order"))
    else
      Right((order.id <-: OrderDetachable) :: Nil)
}
