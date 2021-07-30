package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.data.execution.workflow.context.StateView
import js7.data.execution.workflow.instructions.ForkInstructionExecutor.{checkOrderIdCollisions, toJoined}
import js7.data.order.OrderEvent.{OrderFailedIntermediate_, OrderForked, OrderMoved}
import js7.data.order.{Order, Outcome}
import js7.data.workflow.instructions.Fork

private[instructions] final class ForkExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor
{
  type Instr = Fork

  def toEvents(fork: Fork, order: Order[Order.State], state: StateView) = {
    start(order)
      .getOrElse(Checked(order
        .ifState[Order.Ready].map(order =>
          checkOrderIdCollisions(state,
            order.id <-: OrderForked(
              for (branch <- fork.branches) yield
                OrderForked.Child(branch.id, order.id | branch.id.string))))
        .orElse(
          for {
            order <- order.ifState[Order.Forked]
            joined <- toJoined(state, order)
          } yield joined)
        .orElse(order.ifState[Order.Processed].map(order =>
          order.id <-: (
            order.lastOutcome match {
              case _: Outcome.Succeeded =>
                OrderMoved(order.position.increment)

              case _ =>
                OrderFailedIntermediate_()
            })))
        .toList))
  }
}
