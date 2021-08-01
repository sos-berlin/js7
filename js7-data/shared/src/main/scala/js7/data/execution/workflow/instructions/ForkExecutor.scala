package js7.data.execution.workflow.instructions

import cats.instances.list._
import cats.syntax.traverse._
import js7.data.execution.workflow.context.StateView
import js7.data.order.OrderEvent.{OrderFailedIntermediate_, OrderForked, OrderMoved}
import js7.data.order.{Order, Outcome}
import js7.data.workflow.instructions.Fork

private[instructions] final class ForkExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor with ForkInstructionExecutor
{
  type Instr = Fork

  def toEvents(fork: Fork, order: Order[Order.State], state: StateView) =
    start(order)
      .getOrElse(order
        .ifState[Order.Ready].map { order =>
          val orderForked = OrderForked(
            for (branch <- fork.branches.toVector) yield
              OrderForked.Child(branch.id, order.id | branch.id.string))
          for (_ <- postprocessOrderForked(order, orderForked, state)) yield
            order.id <-: orderForked
        }
        .orElse(
          for {
            order <- order.ifState[Order.Forked]
            joined <- toJoined(order, state)
          } yield Right(joined))
        .orElse(order.ifState[Order.Processed].map(order =>
          Right(
            order.id <-: (
              order.lastOutcome match {
                case _: Outcome.Succeeded =>
                  OrderMoved(order.position.increment)

                case _ =>
                  OrderFailedIntermediate_()
              }))))
        .toList
        .sequence)
}
