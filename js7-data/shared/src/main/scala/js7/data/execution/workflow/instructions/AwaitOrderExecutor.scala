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

  def toEvents(instruction: AwaitOrder, order: Order[Order.State], context: OrderContext) =
    Right(
      order.ifState[Order.Fresh].map(order =>
        order.id <-: OrderStarted)
      .orElse(
        order.ifState[Order.Ready].map(
          _.id <-: OrderAwaiting(instruction.orderId)))
      .orElse(
          for {
            order <- order.ifState[Order.Awaiting]
            _ <- context.idToOrder(instruction.orderId).toOption.flatMap(_.ifState[Order.Offering])
          } yield
            order.id <-: OrderJoined(Outcome.succeeded))
      .orElse(
        ifProcessedThenOrderMoved(order, context))
      .toList)
}
