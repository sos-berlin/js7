package js7.data.workflow.instructions

import js7.data.board.{BoardPath, BoardPathExpression}
import js7.data.order.OrderEvent.{OrderMoved, OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderNoticesRead}
import js7.data.order.{Order, OrderEvent}
import js7.data.state.StateView

trait ExpectOrConsumeNoticesInstruction extends NoticeInstruction:

  def boardPaths: BoardPathExpression

  def fulfilledEvents(
    order: Order[Order.Ready | Order.ExpectingNotices],
    expected: Vector[OrderNoticesExpected.Expected])
  : List[OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved]

  final def tryFulfillExpectingOrder(
    order: Order[Order.ExpectingNotices],
    state: StateView,
    postedBoards: Set[BoardPath] = Set.empty)
  : List[OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved] =
    tryFulfill(order, order.state.expected, state, postedBoards)

  final def tryFulfill(
    order: Order[Order.Ready | Order.ExpectingNotices],
    expected: Vector[OrderNoticesExpected.Expected],
    state: StateView,
    postedBoards: Set[BoardPath] = Set.empty)
  : List[OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved] =
    if isFulfilled(postedBoards ++ state.availableNotices(expected)) then
      fulfilledEvents(order, expected)
    else
      Nil

  private def isFulfilled(isNoticeAvailable: BoardPath => Boolean): Boolean =
    boardPaths.eval(isNoticeAvailable)
