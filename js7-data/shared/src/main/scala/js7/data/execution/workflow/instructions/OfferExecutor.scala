package js7.data.execution.workflow.instructions

import js7.base.time.Timestamp
import js7.data.execution.workflow.context.OrderContext
import js7.data.execution.workflow.instructions.InstructionExecutor.ifProcessedThenOrderMoved
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderOffered, OrderStarted}
import js7.data.workflow.instructions.Offer

/**
  * @author Joacim Zschimmer
  */
object OfferExecutor extends EventInstructionExecutor
{
  type Instr = Offer

  def toEvents(instruction: Offer, order: Order[Order.State], context: OrderContext) =
    Right(
      order.ifState[Order.Fresh].map(order =>
        order.id <-: OrderStarted)
      .orElse(
        order.ifState[Order.Ready].map(
          _.id <-: OrderOffered(instruction.orderId, Timestamp.now + instruction.timeout))
        .orElse(
          ifProcessedThenOrderMoved(order, context)))
      .toList)
}
