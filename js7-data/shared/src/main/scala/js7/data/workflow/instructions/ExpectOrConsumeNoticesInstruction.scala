package js7.data.workflow.instructions

import js7.data.board.{BoardPath, BoardPathExpression}
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderActorEvent, OrderNoticesConsumptionStarted}

trait ExpectOrConsumeNoticesInstruction extends BoardInstruction
{
  def boardPaths: BoardPathExpression

  def isFulfilled(isNoticeAvailable: BoardPath => Boolean): Boolean =
    boardPaths.eval(isNoticeAvailable)

  def fulfilledEvents(
    order: Order[Order.State],
    consuming: Vector[OrderNoticesConsumptionStarted.Consuming])
  : List[OrderActorEvent]
}
