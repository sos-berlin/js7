package js7.data.execution.workflow.instructions

import js7.data.order.Order
import js7.data.order.Order.IsFreshOrReady
import js7.data.order.OrderEvent.{OrderMoved, OrderSaid}
import js7.data.state.EngineState
import js7.data.workflow.instructions.Say

private[instructions] final class SayExecutor(
  protected val service: InstructionExecutorService)
extends EventInstructionExecutor:

  type Instr = Say
  val instructionClass = classOf[Say]

  def toEvents(instr: Say, order: Order[Order.State], state: EngineState) =
    if !order.isState[IsFreshOrReady] then
      Right(Nil)
    else
      start(order).getOrElse:
        for
          scope <- state.toOrderScope(order)
          value <- instr.what.eval(using scope)
        yield
          (order.id <-: OrderSaid(value)) ::
            (order.id <-: OrderMoved(order.position.increment)) ::
            Nil
