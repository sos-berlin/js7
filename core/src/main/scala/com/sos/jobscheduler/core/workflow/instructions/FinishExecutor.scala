package com.sos.jobscheduler.core.workflow.instructions

import cats.data.Validated.Valid
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.{OrderDetachable, OrderFinished, OrderStarted}
import com.sos.jobscheduler.data.workflow.instructions.Finish

/**
  * @author Joacim Zschimmer
  */
object FinishExecutor extends EventInstructionExecutor
{
  type Instr = Finish

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: Finish) =
    order.state match {
      case _: Order.Fresh =>
        Valid(Some(order.id <-: OrderStarted))

      case _: Order.Ready =>
        if (order.isAttached)
          Valid(Some(order.id <-: OrderDetachable))
        else
          Valid(Some(order.id <-: OrderFinished))

      case _ => Valid(None)
    }
}
