package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.data.order.Order
import js7.data.order.Order.IsFreshOrReady
import js7.data.order.OrderEvent.{OrderMoved, OrderSaid}
import js7.data.state.StateView
import js7.data.workflow.instructions.Say

private[instructions] final class SayExecutor(
  protected val service: InstructionExecutorService)
extends EventInstructionExecutor:

  type Instr = Say
  val instructionClass = classOf[Say]

  def toEvents(instr: Say, order: Order[Order.State], state: StateView) =
    if !order.isState[IsFreshOrReady] then
      Right(Nil)
    else
      for
        scope <- state.toOrderScope(order)
        value <- instr.what.eval(using scope)
      yield
        (order.id <-: OrderSaid(value)) ::
          (order.id <-: OrderMoved(order.position.increment)) ::
          Nil
