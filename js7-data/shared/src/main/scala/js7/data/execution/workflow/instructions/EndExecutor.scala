package js7.data.execution.workflow.instructions

import js7.data.execution.workflow.context.OrderContext
import js7.data.execution.workflow.instructions.InstructionExecutor.instructionToExecutor
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderDetachable, OrderFinished, OrderMoved, OrderStarted}
import js7.data.workflow.instructions.{End, Fork, LockInstruction}

/**
  * @author Joacim Zschimmer
  */
object EndExecutor extends EventInstructionExecutor with PositionInstructionExecutor
{
  type Instr = End

  def toEvents(instruction: End, order: Order[Order.State], context: OrderContext) =
    Right(
      (order.position.dropChild match {
        case None =>
          order.state match {
            case _: Order.Ready =>
              if (order.isAttached)
                (order.id <-: OrderDetachable) :: Nil
              else
                (order.id <-: OrderFinished) :: Nil

            case _: Order.Fresh =>
              if (order.isAttached)
                (order.id <-: OrderDetachable) :: Nil
              else
                (order.id <-: OrderStarted) ::
                (order.id <-: OrderFinished) :: Nil

            case _ => Nil
          }

        case Some(returnPosition) =>
          context.instruction(order.workflowId /: returnPosition) match {
            case fork: Fork =>
              ForkExecutor.tryJoinChildOrder(context, order, fork).toList
            case lock: LockInstruction =>
              LockExecutor.onReturnFromSubworkflow(order, lock).toList
            case _ =>
              (order.id <-: OrderMoved(returnPosition.increment)) :: Nil
          }
      }))

  def nextPosition(instruction: End, order: Order[Order.State], context: OrderContext) =
    Right(
      for {
        returnPosition <- order.position.dropChild
        next <- instructionToExecutor(context.instruction(order.workflowId /: returnPosition)) match {
          case ForkExecutor => None
          case LockExecutor => None
          case _: PositionInstructionExecutor => Some(returnPosition.increment)  // Check this first for TryInstruction !!!
          case _: EventInstructionExecutor => Some(returnPosition)
          case _ => None
        }
      } yield next)
}
