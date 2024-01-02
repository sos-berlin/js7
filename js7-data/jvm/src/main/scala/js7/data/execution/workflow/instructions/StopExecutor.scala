package js7.data.execution.workflow.instructions

import js7.data.order.Order
import js7.data.order.OrderEvent.OrderStopped
import js7.data.state.StateView
import js7.data.workflow.instructions.Stop

private[instructions] final class StopExecutor(
  protected val service: InstructionExecutorService)
extends EventInstructionExecutor:

  type Instr = Stop
  val instructionClass = classOf[Stop]

  def toEvents(instr: Stop, order: Order[Order.State], state: StateView) =
    detach(order)
      .orElse(
        start(order))
      .getOrElse(Right(
        (order.id <-: OrderStopped) :: Nil))
