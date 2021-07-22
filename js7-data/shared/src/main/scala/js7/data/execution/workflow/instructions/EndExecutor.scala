package js7.data.execution.workflow.instructions

import js7.data.execution.workflow.context.StateView
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderDetachable, OrderFinished, OrderMoved, OrderStarted}
import js7.data.workflow.instructions.{End, Fork, LockInstruction}

private[instructions] final class EndExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor with PositionInstructionExecutor
{
  type Instr = End

  def toEvents(instruction: End, order: Order[Order.State], state: StateView) =
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
          state.instruction(order.workflowId /: returnPosition) match {
            case fork: Fork =>
              service.forkExecutor.tryJoinChildOrder(state, order, fork).toList
            case lock: LockInstruction =>
              service.lockExecutor.onReturnFromSubworkflow(order, lock).toList
            case _ =>
              (order.id <-: OrderMoved(returnPosition.increment)) :: Nil
          }
      }))

  def nextPosition(instruction: End, order: Order[Order.State], state: StateView) = {
    import service.instructionToExecutor
    Right(
      for {
        returnPosition <- order.position.dropChild
        next <- instructionToExecutor(state.instruction(order.workflowId /: returnPosition)) match {
          case _: ForkExecutor => None
          case _: LockExecutor => None
          case _: PositionInstructionExecutor => Some(returnPosition.increment)  // Check this first for TryInstruction !!!
          case _: EventInstructionExecutor => Some(returnPosition)
          case _ => None
        }
      } yield next)
  }
}
