package js7.data.execution.workflow.instructions

import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderFinished, OrderMoved}
import js7.data.state.StateView
import js7.data.workflow.instructions.End

private[instructions] final class EndExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor with PositionInstructionExecutor:
  type Instr = End
  val instructionClass = classOf[End]

  import service.instructionToExecutor

  def toEvents(instruction: End, order: Order[Order.State], state: StateView) =
    order.position.parent
      .filter(_ => order.position.branchPath != order.innerBlock)
      .match
        case None =>
          order.state match
            case _: Order.IsFreshOrReady =>
              detach(order)
                .orElse(
                  start(order))
                .getOrElse(Right(
                  (order.id <-: OrderFinished()) :: Nil))

            case _ => Right(Nil)

        case Some(parentPos) =>
          val instr = state.instruction(order.workflowId /: parentPos)
          service.onReturnFromSubworkflow(instr, order, state)

  def nextMove(instruction: End, order: Order[Order.State], state: StateView) =
    Right(
      if order.position.branchPath == order.innerBlock then
        None
      else
        for
          parentPos <- order.position.parent
          exec = instructionToExecutor(state.instruction(order.workflowId /: parentPos))
          next <- exec.subworkflowEndToPosition(parentPos)
        yield OrderMoved(next))
