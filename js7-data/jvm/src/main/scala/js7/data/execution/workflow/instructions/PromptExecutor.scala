package js7.data.execution.workflow.instructions

import js7.data.order.Order
import js7.data.order.Order.Ready
import js7.data.order.OrderEvent.OrderPrompted
import js7.data.state.StateView
import js7.data.workflow.instructions.Prompt

private[instructions] final class PromptExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor
{
  type Instr = Prompt
  val instructionClass = classOf[Prompt]

  def toEvents(prompt: Prompt, order: Order[Order.State], state: StateView) =
    detach(order)
      .orElse(start(order))
      .orElse(order
        .ifState[Ready].map(_ =>
          for {
            scope <- state.toImpureOrderExecutingScope(order, clock.now())
            question <- prompt.question.eval(scope)
          } yield (order.id <-: OrderPrompted(question)) :: Nil))
      .getOrElse(Right(Nil))
}
