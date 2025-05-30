package js7.data.board

import io.circe.Codec
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec
import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.data.board.PlannableBoard.*
import js7.data.item.ItemRevision
import js7.data.order.{FreshOrder, Order}
import js7.data.state.StateView
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.{Expression, Scope}

/** A PlannableBoard is a BoardItem with NoticeIds for each PlanId.
  */
final case class PlannableBoard(
  path: BoardPath,
  postOrderToNoticeKey: Expression = DefaultToNoticeExpr,
  expectOrderToNoticeKey: Expression = DefaultToNoticeExpr,
  itemRevision: Option[ItemRevision] = None)
extends
  BoardItem:

  protected type Self = PlannableBoard

  val companion: PlannableBoard.type = PlannableBoard

  def rename(path: BoardPath): PlannableBoard =
    copy(path = path)

  def withRevision(revision: Option[ItemRevision]): PlannableBoard =
    copy(itemRevision = revision)

  def isGlobal: Boolean =
    false

  def freshOrderToNoticeKey(order: FreshOrder, state: StateView)
  : Checked[NoticeKey] =
    val scope = state.toPlanOrderScope(order)
    postingOrderToNoticeKey(scope)

  def postingOrderToNotice(order: Order[Order.Ready], state: StateView): Checked[Notice] =
    val scope = state.toPlanOrderScope(order)
    for
      noticeKey <- postingOrderToNoticeKey(scope)
      notice <- toNotice(order.planId / noticeKey)(scope)
    yield
      notice

  private def postingOrderToNoticeKey(scope: Scope): Checked[NoticeKey] =
    for
      noticeKey <- postOrderToNoticeKey.evalAsString(using scope)
      noticeKey <- NoticeKey.checked(noticeKey)
    yield
      noticeKey

  def expectingOrderToNoticeId(order: Order[Order.Ready], state: StateView): Checked[NoticeId] =
    val scope = state.toPlanOrderScope(order)
    for
      noticeKey <- expectOrderToNoticeKey.evalAsString(using scope)
    yield
      order.planId / path / noticeKey

  protected def evalEndOfLife(scope: Scope): Checked[Option[Timestamp]] =
    Right(None)


object PlannableBoard extends BoardItem.Companion[PlannableBoard]:

  type ItemState = BoardState

  val Path: BoardPath.type = BoardPath

  /** Returns the empty String, the empty NoticeKey. */
  val DefaultToNoticeExpr: StringConstant =
    StringConstant("")

  def cls: Class[PlannableBoard] =
    classOf[PlannableBoard]

  given jsonCodec: Codec.AsObject[PlannableBoard] = deriveConfiguredCodec
