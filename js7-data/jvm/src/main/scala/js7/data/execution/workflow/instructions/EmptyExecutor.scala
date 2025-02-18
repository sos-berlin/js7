package js7.data.execution.workflow.instructions

import js7.data.order.Order
import js7.data.order.Order.IsFreshOrReady
import js7.data.order.OrderEvent.OrderMoved
import js7.data.state.StateView
import js7.data.workflow.instructions.EmptyInstruction

private[instructions] final class EmptyExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor:

  type Instr = EmptyInstruction
  val instructionClass = classOf[EmptyInstruction]

  def toEvents(instr: EmptyInstruction, order: Order[Order.State], state: StateView) =
    order.ifState[IsFreshOrReady].map: order =>
      Right:
        (order.id <-: OrderMoved(order.position.increment)) :: Nil
    .getOrElse:
      Right(Nil)