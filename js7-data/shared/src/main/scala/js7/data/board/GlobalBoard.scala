package js7.data.board

import cats.syntax.traverse.*
import io.circe.Codec
import io.circe.derivation.ConfiguredCodec
import js7.base.circeutils.CirceUtils.deriveRenamingCodec
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.circeutils.typed.Subtype
import js7.base.problem.Checked
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.item.{ItemRevision, UnsignedItemPath}
import js7.data.order.Order
import js7.data.state.StateView
import js7.data.value.expression.Expression.{MissingConstant, expr}
import js7.data.value.expression.ExpressionParser.parseExpression
import js7.data.value.expression.{Expression, Scope}
import scala.concurrent.duration.FiniteDuration

/** A GlobalBoard is a BoardItem with global NoticeIds.
  *
  * NoticeIds are in the global Plan (PlanId.Global).
  */
final case class GlobalBoard(
  path: BoardPath,
  postOrderToNoticeKey: Expression,
  expectOrderToNoticeKey: Expression,
  endOfLife: Expression,
  itemRevision: Option[ItemRevision] = None)
extends
  BoardItem:

  protected type Self = GlobalBoard
  val companion: GlobalBoard.type = GlobalBoard

  def withRevision(revision: Option[ItemRevision]): GlobalBoard =
    copy(itemRevision = revision)

  def rename(path: BoardPath): GlobalBoard =
    copy(path = path)

  def isGlobal: Boolean =
    true

  def postingOrderToNotice(order: Order[Order.Ready], state: StateView, now: Timestamp)
  : Checked[Notice] =
    for
      // Does this Scope make sense??? It differs from scope in expectingOrderToNoticeId.
      scope <- state.toImpureOrderExecutingScope(order, now)
      noticeKey <- postOrderToNoticeKey.evalAsString(scope)
      plannedNoticeKey <- GlobalNoticeKey.checked(noticeKey)
      notice <- toNotice(plannedNoticeKey)(scope)
    yield
      notice

  def expectingOrderToNoticeId(order: Order[Order.Ready], state: StateView, now: Timestamp)
  : Checked[NoticeId] =
    for
      scope <- state.toOrderScope(order)
      noticeKey <- expectOrderToNoticeKey.evalAsString(scope)
      plannedNoticeKey <- GlobalNoticeKey.checked(noticeKey)
    yield
      path / plannedNoticeKey

  def evalEndOfLife(scope: Scope): Checked[Option[Timestamp]] =
    endOfLife
      .eval(scope)
      .map(_.missingToNone)
      .flatMap(_.traverse(_.asLongIgnoreFraction))
      .map(_.map(Timestamp.ofEpochMilli))


object GlobalBoard extends BoardItem.Companion[GlobalBoard]:

  val cls: Class[GlobalBoard] = classOf[GlobalBoard]

  override type Path = BoardPath
  override val Path: UnsignedItemPath.Companion[BoardPath] = BoardPath

  type ItemState = BoardState

  given jsonCodec: Codec.AsObject[GlobalBoard] = deriveRenamingCodec(Map(
    "postOrderToNoticeId" -> "postOrderToNoticeKey",
    "expectOrderToNoticeId" -> "expectOrderToNoticeKey"))

  override val subtype: Subtype[GlobalBoard] =
    Subtype[GlobalBoard](aliases = Seq("Board"/*until v2.7.2*/))

  /** A GlobalBoard with a single, constant Notice. */
  def singleNotice(
    boardPath: BoardPath,
    orderToNoticeId: String = "'NOTICE'",
    lifetime: FiniteDuration = 24.h)
  : GlobalBoard =
    GlobalBoard(
      boardPath,
      postOrderToNoticeKey = parseExpression(orderToNoticeId).orThrow,
      expectOrderToNoticeKey = parseExpression(orderToNoticeId).orThrow,
      endOfLife = parseExpression("$js7EpochMilli + " + lifetime.toMillis).orThrow)

  private val sosOrderToNotice =
    expr""" match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$$1') """

  /** A GlobalBoard for JOC-style OrderIds. */
  def joc(boardPath: BoardPath, lifetime: Option[FiniteDuration] = Some(24.h)): GlobalBoard =
    GlobalBoard(
      boardPath,
      postOrderToNoticeKey = sosOrderToNotice,
      expectOrderToNoticeKey = sosOrderToNotice,
      endOfLife = lifetime.fold(MissingConstant):
        lifetime => parseExpression("$js7EpochMilli + " + lifetime.toMillis).orThrow)

  //<editor-fold desc="// General JOC OrderId pattern">
  // General JOC OrderId pattern: #DATE#(T|P|D|F|C)[\d-]-NAME(|BRANCHID)? (Olli)

  //private val dailyPlanDateAndOrderName = expr:
  //  "match(orderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*-([^:]*)(?::[^|]*)?([|].*)?$', '$1$2$3')"
  //
  //private val matchingDailyPlanDate = expr:
  //  "match(orderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', '$1')"
  //
  //private val matchingOrderName = expr:
  //  "match(orderId, '^#[0-9]{4}-[0-9]{2}-[0-9]{2}#.*-([^:]*)(?::[^|]*)?([|].*)?$', '$1$2')"
  //
  ///** A GlobalBoard for JOC-style OrderIds. */
  //def joc2(boardPath: BoardPath, lifetime: Option[FiniteDuration] = Some(24.h))
  //: GlobalBoard =
  //  GlobalBoard(
  //    boardPath,
  //    postOrderToNoticeKey = dailyPlanDateAndOrderName,
  //    expectOrderToNoticeKey = dailyPlanDateAndOrderName,
  //    endOfLife = lifetime.fold(MissingConstant):
  //      lifetime => expr("$js7EpochMilli + " + lifetime.toMillis))
  //</editor-fold>
