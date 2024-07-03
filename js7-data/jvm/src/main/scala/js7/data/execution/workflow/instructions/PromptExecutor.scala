package js7.data.execution.workflow.instructions

import js7.data.order.Order.Ready
import js7.data.order.OrderEvent.OrderPrompted
import js7.data.order.OrderObstacle.WaitingForCommand
import js7.data.order.{Order, OrderObstacleCalculator}
import js7.data.state.StateView
import js7.data.value.GoodValue
import js7.data.workflow.instructions.Prompt

private[instructions] final class PromptExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor:

  type Instr = Prompt
  val instructionClass = classOf[Prompt]

  def toEvents(prompt: Prompt, order: Order[Order.State], state: StateView) =
    detach(order)
      .orElse:
        start(order)
      .orElse(order
        .ifState[Ready].map: _ =>
          for
            scope <- state.toImpureOrderExecutingScope(order, clock.now())
            question <- prompt.question.evalAs(GoodValue.companion, scope)
          yield
            (order.id <-: OrderPrompted(question)) :: Nil)
      .getOrElse:
        Right(Nil)

  override def toObstacles(order: Order[Order.State], calculator: OrderObstacleCalculator) =
    order.state match
      case Order.Prompting(_) =>
        Right(Set(WaitingForCommand))

      case _ =>
        super.toObstacles(order, calculator)
