package com.sos.jobscheduler.core.workflow.instructions

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.Logger.ops.RichScalaLogger
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderRetrying}
import com.sos.jobscheduler.data.workflow.instructions.Retry

/**
  * @author Joacim Zschimmer
  */
object RetryExecutor extends EventInstructionExecutor
{
  type Instr = Retry

  private val logger = Logger(getClass)

  def toEvent(context: OrderContext, order: Order[Order.State], retry: Retry): Option[KeyedEvent[OrderActorEvent]] =
  {
    context.toRetryPosition(order.workflowPosition)
      match {
        case Invalid(problem) => logger.error(problem); None
        case Valid(pos) => Some(order.id <-: OrderRetrying(pos))
      }
  }
}
