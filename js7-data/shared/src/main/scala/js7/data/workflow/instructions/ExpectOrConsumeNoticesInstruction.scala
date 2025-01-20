package js7.data.workflow.instructions

import io.circe.Codec
import js7.base.circeutils.CirceUtils.enumCodec
import js7.base.utils.L3
import js7.data.board.{BoardNoticeKey, BoardPath, BoardPathExpression}
import js7.data.order.OrderEvent.{OrderMoved, OrderNoticesConsumptionStarted, OrderNoticesRead}
import js7.data.order.{Order, OrderEvent}

trait ExpectOrConsumeNoticesInstruction extends NoticeInstruction:

  def boardPaths: BoardPathExpression

  protected def fulfilledEvents(
    order: Order[Order.Ready | Order.ExpectingNotices],
    boardNoticeKeys: Vector[BoardNoticeKey],
    exprResult: L3)
  : List[OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved]

  final def tryFulfillExpectingOrder(
    order: Order[Order.ExpectingNotices],
    isNoticeAvailable: BoardNoticeKey => L3,
    postedNoticeIds: Set[BoardNoticeKey] = Set.empty)
  : List[OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved] =
    tryFulfill(order, order.state.boardNoticeKeys, isNoticeAvailable, postedNoticeIds)

  final def tryFulfill(
    order: Order[Order.Ready | Order.ExpectingNotices],
    boardNoticeKeys: Vector[BoardNoticeKey],
    isNoticeAvailable: BoardNoticeKey => L3,
    postedNoticeIds: Set[BoardNoticeKey] = Set.empty)
  : List[OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved] =
    val boardToL3: BoardPath => L3 =
      boardNoticeKeys.map: boardNoticeKey =>
        boardNoticeKey.boardPath -> locally:
          if postedNoticeIds(boardNoticeKey) then
            L3.True
          else
            isNoticeAvailable(boardNoticeKey)
      .toMap
    if boardPaths.eval(boardToL3) != L3.False then
      val consumption = boardNoticeKeys.filter(o => postedNoticeIds(o) || isNoticeAvailable(o) == L3.True)
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
    given Codec[WhenNotAnnounced] = enumCodec(valueOf, values)
