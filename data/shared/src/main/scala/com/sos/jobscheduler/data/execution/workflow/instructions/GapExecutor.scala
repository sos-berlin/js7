package js7.data.execution.workflow.instructions

import js7.base.problem.Problem
import js7.data.execution.workflow.context.OrderContext
import js7.data.order.Order
import js7.data.order.OrderEvent.OrderDetachable
import js7.data.workflow.instructions.Gap

/**
  * @author Joacim Zschimmer
  */
object GapExecutor extends EventInstructionExecutor {

  type Instr = Gap

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: Gap) =
    if (!order.isAttached)
      Left(Problem.pure(s"Instruction Gap but order is not attached to an agent: $order"))
    else
      Right(Some(order.id <-: OrderDetachable))
}
