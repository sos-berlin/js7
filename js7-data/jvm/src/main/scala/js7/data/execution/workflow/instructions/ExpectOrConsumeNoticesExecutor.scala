package js7.data.execution.workflow.instructions

import js7.data.board.BoardPath
import js7.data.order.OrderEvent.OrderNoticesExpected
import js7.data.order.{Order, OrderEvent}
import js7.data.state.StateView
import js7.data.workflow.instructions.ExpectOrConsumeNoticesInstruction

object ExpectOrConsumeNoticesExecutor:
  
  def tryFulfillExpectingOrder(
    expectNotices: ExpectOrConsumeNoticesInstruction,
    order: Order[Order.ExpectingNotices],
    state: StateView,
    postedBoards: Set[BoardPath] = Set.empty)
  : List[OrderEvent.OrderActorEvent] =
    tryFulfill(expectNotices, order, order.state.expected, state, postedBoards)

  private[instructions] def tryFulfill(
    expectNotices: ExpectOrConsumeNoticesInstruction,
    order: Order[Order.State],
    expected: Vector[OrderNoticesExpected.Expected],
    state: StateView,
    postedBoards: Set[BoardPath] = Set.empty)
  : List[OrderEvent.OrderActorEvent] =
    if expectNotices.isFulfilled(postedBoards ++ state.availableNotices(expected)) then
      expectNotices.fulfilledEvents(order, expected)
    else
      Nil
