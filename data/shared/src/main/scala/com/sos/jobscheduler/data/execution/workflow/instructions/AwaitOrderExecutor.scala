package js7.data.execution.workflow.instructions

import js7.data.execution.workflow.context.OrderContext
import js7.data.execution.workflow.instructions.InstructionExecutor.ifProcessedThenOrderMoved
import js7.data.order.OrderEvent.{OrderAwaiting, OrderJoined, OrderStarted}
import js7.data.order.{Order, Outcome}
import js7.data.workflow.instructions.AwaitOrder

/**
  * @author Joacim Zschimmer
  */
object AwaitOrderExecutor extends EventInstructionExecutor
{
  type Instr = AwaitOrder

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: AwaitOrder) =
    Right(
      order.ifState[Order.Fresh].map(order =>
        order.id <-: OrderStarted)
      .orElse(
        order.ifState[Order.Ready].map(
          _.id <-: OrderAwaiting(instruction.orderId)))
      .orElse(
          for {
            order <- order.ifState[Order.Awaiting]
            _ <- context.idToOrder.lift(instruction.orderId) flatMap (_.ifState[Order.Offering])
          } yield
            order.id <-: OrderJoined(Outcome.succeeded))
      .orElse(
        ifProcessedThenOrderMoved(order, context)))
}
