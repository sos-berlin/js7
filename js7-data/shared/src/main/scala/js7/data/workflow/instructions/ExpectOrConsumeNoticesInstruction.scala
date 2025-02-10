package js7.data.workflow.instructions

import io.circe.Codec
import js7.base.circeutils.CirceUtils.enumCodec
import js7.base.utils.L3
import js7.data.board.{BoardPathExpression, NoticeId}
import js7.data.order.OrderEvent.{OrderMoved, OrderNoticesConsumptionStarted, OrderNoticesRead}
import js7.data.order.{Order, OrderEvent}

trait ExpectOrConsumeNoticesInstruction extends NoticeInstruction:

  def boardPaths: BoardPathExpression

  protected def fulfilledEvents(
    order: Order[Order.Ready | Order.ExpectingNotices],
    noticeIds: Vector[NoticeId],
    consumedNoticeIds: Vector[NoticeId],
    exprResult: L3)
  : List[OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved]

  final def tryFulfillExpectingOrder(
    order: Order[Order.ExpectingNotices],
    isNoticeAvailable: NoticeId => L3,
    isPostedNow: Set[NoticeId] = Set.empty)
  : List[OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved] =
    tryFulfill(order, order.state.noticeIds, isNoticeAvailable, isPostedNow)

  final def tryFulfill(
    order: Order[Order.Ready | Order.ExpectingNotices],
    noticeIds: Vector[NoticeId],
    isNoticeAvailable: NoticeId => L3,
    isPostedNow: Set[NoticeId] = Set.empty)
  : List[OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved] =
    val boardToL3 =
      noticeIds.map: noticeId =>
        noticeId.boardPath -> locally:
          if isPostedNow(noticeId) then
            L3.True
          else
            isNoticeAvailable(noticeId)
      .toMap
    if boardPaths.eval(boardToL3) != L3.False then
      val consumedNoticeIds = noticeIds.filter:
        o => isPostedNow(o) || isNoticeAvailable(o) == L3.True
      fulfilledEvents(order, noticeIds, consumedNoticeIds, boardPaths.eval(boardToL3))
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
