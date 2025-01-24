package js7.data.board

import io.circe.Codec
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec
import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.data.board.PlannableBoard.*
import js7.data.item.ItemRevision
import js7.data.order.{FreshOrder, Order}
import js7.data.plan.PlanId
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

  def freshOrderToNoticeKey(planId: PlanId, order: FreshOrder, state: StateView)
  : Checked[PlannedNoticeKey] =
    val scope = state.toPlanOrderScope(order)
    postingOrderToNoticeKey(planId, scope)

  def postingOrderToNotice(order: Order[Order.Ready], state: StateView): Checked[Notice] =
    val scope = state.toPlanOrderScope(order)
    for
      plannedNoticeKey <- postingOrderToNoticeKey(order.planId, scope)
      notice <- toNotice(plannedNoticeKey)(scope)
    yield
      notice

  private def postingOrderToNoticeKey(planId: PlanId, scope: Scope)
  : Checked[PlannedNoticeKey] =
    for
      noticeKey <- postOrderToNoticeKey.evalAsString(scope)
      plannedNoticeKey <- PlannedNoticeKey.checked(planId, noticeKey)
    yield
      plannedNoticeKey

  def expectingOrderToNoticeId(order: Order[Order.Ready], state: StateView): Checked[NoticeId] =
    val scope = state.toPlanOrderScope(order)
    for
      noticeKey <- expectOrderToNoticeKey.evalAsString(scope)
      _ <- order.planId.nonGlobal
    yield
      order.planId / path / noticeKey

  protected def evalEndOfLife(scope: Scope): Checked[Option[Timestamp]] =
    Right(None)


object PlannableBoard extends BoardItem.Companion[PlannableBoard]:

  type ItemState = BoardState

  val Path: BoardPath.type = BoardPath

  /** Returns the empty String, the empty NoticeKey. */
  val DefaultToNoticeExpr = StringConstant("")

  def cls: Class[PlannableBoard] =
    classOf[PlannableBoard]

  given jsonCodec: Codec.AsObject[PlannableBoard] = deriveConfiguredCodec
