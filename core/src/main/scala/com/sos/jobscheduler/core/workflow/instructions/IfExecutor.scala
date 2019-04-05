package com.sos.jobscheduler.core.workflow.instructions

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.OrderMoved
import com.sos.jobscheduler.data.workflow.instructions.If
import com.sos.jobscheduler.data.workflow.position.BranchId.{Else, Then}

/**
  * @author Joacim Zschimmer
  */
object IfExecutor extends EventInstructionExecutor with PositionInstructionExecutor
{
  type Instr = If

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: If) =
    if (order.isState[Order.Broken] || order.isState[Order.StoppedWhileFresh] || order.isState[Order.Stopped])
      Valid(None)
    else
      nextPosition(context, order, instruction)
        .map(_ map (o => order.id <-: OrderMoved(o)))

  def nextPosition(context: OrderContext, order: Order[Order.State], instruction: If) = {
    assert(order == context.idToOrder(order.id).withPosition(order.position))
    context.makeScope(order).evalBoolean(instruction.predicate)
      .map {
        case true => Some(Then)
        case false => instruction.elseWorkflow.isDefined ? Else
      }
      .map {
        case Some(thenOrElse) => Some(order.position / thenOrElse % 0)
        case None => Some(order.position.increment)  // No else-part, skip instruction
      }
  }
}
