package com.sos.jobscheduler.data.execution.workflow.instructions

import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.execution.workflow.context.OrderContext
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.OrderDetachable
import com.sos.jobscheduler.data.workflow.instructions.Gap

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
