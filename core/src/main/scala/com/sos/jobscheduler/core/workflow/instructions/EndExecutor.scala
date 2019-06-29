package com.sos.jobscheduler.core.workflow.instructions

import cats.data.Validated.Valid
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.core.workflow.instructions.InstructionExecutor.instructionToExecutor
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.{OrderDetachable, OrderFinished, OrderMoved}
import com.sos.jobscheduler.data.workflow.instructions.{End, Fork}

/**
  * @author Joacim Zschimmer
  */
object EndExecutor extends EventInstructionExecutor with PositionInstructionExecutor
{
  type Instr = End

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: End) =
    Valid(
      order.position.dropChild match {
        case None =>
          for (order <- order.ifState[Order.Ready]) yield
            if (order.isAttached)
              order.id <-: OrderDetachable
            else
              order.id <-: OrderFinished

      case Some(returnPosition) =>
        context.instruction(order.workflowId /: returnPosition) match {
          case fork: Fork =>
            ForkExecutor.tryJoinChildOrder(context, order, fork)
          case _ =>
            Some(order.id <-: OrderMoved(returnPosition.increment))
        }
    })

  def nextPosition(context: OrderContext, order: Order[Order.State], instruction: End) =
    Valid(
      for {
        returnPosition <- order.position.dropChild
        next <- instructionToExecutor(context.instruction(order.workflowId /: returnPosition)) match {
          case ForkExecutor => None
          case _: PositionInstructionExecutor => Some(returnPosition.increment)  // Check this first for TryInstruction !!!
          case _: EventInstructionExecutor => Some(returnPosition)
          case _ => None
        }
      } yield next)
}
