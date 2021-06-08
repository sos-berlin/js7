package js7.data.execution.workflow.instructions

import js7.data.execution.workflow.context.StateView
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderDetachable, OrderPrompted, OrderStarted}
import js7.data.workflow.instructions.Prompt

object PromptExecutor extends EventInstructionExecutor
{
  type Instr = Prompt

  def toEvents(prompt: Prompt, order: Order[Order.State], state: StateView) =
    order.state match {
      case _: Order.Fresh =>
        Right((order.id <-: OrderStarted) :: Nil)

      case _: Order.Ready =>
        if (order.isAttached)
          Right((order.id <-: OrderDetachable) :: Nil)
        for {
          scope <- state.makeScope(order)
          question <- scope.evaluator.eval(prompt.question)
        } yield
          (order.id <-: OrderPrompted(question)) :: Nil

      case _ => Right(Nil)
    }
}
