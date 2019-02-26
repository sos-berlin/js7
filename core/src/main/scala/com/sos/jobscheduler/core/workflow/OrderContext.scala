package com.sos.jobscheduler.core.workflow

import com.sos.jobscheduler.core.expression.Scope
import com.sos.jobscheduler.core.workflow.OrderContext._
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome}

/**
  * @author Joacim Zschimmer
  */
trait OrderContext extends InstructionContext
{
  def idToOrder: PartialFunction[OrderId, Order[Order.State]]

  def childOrderEnded(order: Order[Order.State]): Boolean

  final def makeScope(order: Order[Order.State]): Scope =
    new Scope {
      lazy val returnCode = order.outcome match {
        case Outcome.Undisrupted(rc, _) => rc
        case _: Outcome.Disrupted => DisruptedReturnCode
      }

      lazy val retryCount = OrderContext.this.toRetryCount(order.workflowPosition)

      val variableNameToString = order.variables
    }
}

object OrderContext {
  private val DisruptedReturnCode = ReturnCode(-1)  // TODO Should we use this value ?
}
