package js7.data.execution.workflow.instructions

import js7.data.execution.workflow.context.OrderContext
import js7.data.execution.workflow.instructions.InstructionExecutor.instructionToExecutor
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderDetachable, OrderFinished, OrderMoved}
import js7.data.workflow.instructions.{End, Fork}

/**
  * @author Joacim Zschimmer
  */
object EndExecutor extends EventInstructionExecutor with PositionInstructionExecutor
{
  type Instr = End

  def toEvent(instruction: End, order: Order[Order.State], context: OrderContext) =
    Right(
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

  def nextPosition(instruction: End, order: Order[Order.State], context: OrderContext) =
    Right(
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
