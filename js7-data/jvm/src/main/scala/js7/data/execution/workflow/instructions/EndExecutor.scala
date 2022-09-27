package js7.data.execution.workflow.instructions

import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderFinished, OrderMoved}
import js7.data.state.StateView
import js7.data.workflow.instructions.{ConsumeNotices, Cycle, End, ForkInstruction, LockInstruction}

private[instructions] final class EndExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor with PositionInstructionExecutor
{
  type Instr = End
  val instructionClass = classOf[End]

  def toEvents(instruction: End, order: Order[Order.State], state: StateView) =
    start(order)
      .getOrElse(
        (order.position.parent match {
          case None =>
            order.state match {
              case _: Order.Ready =>
                detach(order)
                  .getOrElse(Right(
                    (order.id <-: OrderFinished) :: Nil))

              case _ => Right(Nil)
            }

          case Some(returnPosition) =>
            state.instruction(order.workflowId /: returnPosition) match {
              case fork: ForkInstruction =>
                Right(service.tryJoinChildOrder(fork, order, state).toList)

              case lock: LockInstruction =>
                Right(service.lockExecutor.onReturnFromSubworkflow(order, lock).toList)

              case cycle: Cycle =>
                service.cycleExecutor.onReturnFromSubworkflow(order, cycle, state)
                  .map(_ :: Nil)

              case _: ConsumeNotices =>
                Right(service.consumeNoticesExecutor.onReturnFromSubworkflow(order) :: Nil)

              case _ =>
                Right((order.id <-: OrderMoved(returnPosition.increment)) :: Nil)
            }
        }))

  def nextPosition(instruction: End, order: Order[Order.State], state: StateView) = {
    import service.instructionToExecutor
    Right(
      for {
        returnPosition <- order.position.parent
        next <- instructionToExecutor(state.instruction(order.workflowId /: returnPosition)) match {
          case _: ForkExecutor => None
          case _: ForkListExecutor => None
          case _: LockExecutor => None
          case _: CycleExecutor => None
          case _: ConsumeNoticesExecutor => None
          // Check PositionInstructionExecutor first for TryInstruction !!!
          case _: PositionInstructionExecutor => Some(returnPosition.increment)
          case _: EventInstructionExecutor => Some(returnPosition)
        }
      } yield next)
  }
}
