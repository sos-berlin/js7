package js7.data.workflow.instructions

import io.circe.{Decoder, Encoder}
import js7.base.circeutils.CirceUtils.{enumDecoder, enumEncoder}
import js7.base.utils.L3
import js7.data.board.{BoardPath, BoardPathExpression}
import js7.data.order.OrderEvent.OrderNoticesExpected.Expected
import js7.data.order.OrderEvent.{OrderMoved, OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderNoticesRead}
import js7.data.order.{Order, OrderEvent}

trait ExpectOrConsumeNoticesInstruction extends NoticeInstruction:

  def boardPaths: BoardPathExpression

  protected def fulfilledEvents(
    order: Order[Order.Ready | Order.ExpectingNotices],
    expected: Vector[OrderNoticesExpected.Expected],
    exprResult: L3)
  : List[OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved]

  final def tryFulfillExpectingOrder(
    order: Order[Order.ExpectingNotices],
    isNoticeAvailable: OrderNoticesExpected.Expected => L3,
    postedNoticeIds: Set[Expected] = Set.empty)
  : List[OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved] =
    tryFulfill(order, order.state.expected, isNoticeAvailable, postedNoticeIds)

  final def tryFulfill(
    order: Order[Order.Ready | Order.ExpectingNotices],
    expected: Vector[OrderNoticesExpected.Expected],
    isNoticeAvailable: OrderNoticesExpected.Expected => L3,
    postedNoticeIds: Set[Expected] = Set.empty)
  : List[OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved] =
    val boardToL3: BoardPath => L3 =
      expected.map: expected =>
        expected.boardPath -> locally:
          if postedNoticeIds(expected) then
            L3.True
          else
            isNoticeAvailable(expected)
      .toMap
    if boardPaths.eval(boardToL3) != L3.False then
      val consumption = expected.filter(o => postedNoticeIds(o) || isNoticeAvailable(o) == L3.True)
      fulfilledEvents(order, consumption, boardPaths.eval(boardToL3))
    else
      Nil


object ExpectOrConsumeNoticesInstruction:

  /** How to handle not announced PlannedNoticeKey in ConsumeNotices and ExpectedNotices instructions. */
  enum WhenNotAnnounced:
    /** When required NoticeIds have not been announced, wait nevertheless. */
    case Wait

    /** When required NoticeIds have not been announced, skip the instruction. */
    case SkipWhenNoNotice

    /** When required NoticeIds are ignored (regarded as existent).
      * ConsumeNotices consumes only existent NoticeIds. */
    case DontWait

  object WhenNotAnnounced:
    given Encoder[WhenNotAnnounced] = enumEncoder(values)
    given Decoder[WhenNotAnnounced] = enumDecoder(valueOf)
