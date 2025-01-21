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
import js7.data.item.{ItemRevision, UnsignedItemPath}
import js7.data.value.expression.Expression.MissingConstant
import js7.data.value.expression.ExpressionParser.expr
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

  //def isAnnounced: Boolean =
  //  false

  def postingOrderToNotice(scope: Scope): Checked[Notice] =
    for
      noticeKey <- postOrderToNoticeKey.evalAsString(scope)
      plannedNoticeKey <- PlannedNoticeKey.global(noticeKey)
      notice <- toNotice(plannedNoticeKey)(scope)
    yield
      notice

  protected def evalEndOfLife(scope: Scope): Checked[Option[Timestamp]] =
    endOfLife
      .eval(scope)
      .map(_.missingToNone)
      .flatMap(_.traverse(_.asLongIgnoreFraction))
      .map(_.map(Timestamp.ofEpochMilli))

  def expectingOrderToNoticeId(scope: Scope): Checked[NoticeId] =
    for
      noticeKey <- expectOrderToNoticeKey.evalAsString(scope)
      plannedNoticeKey <- PlannedNoticeKey.global(noticeKey)
    yield
      path / plannedNoticeKey

  def expectingOrderToPlannedNoticeKey(scope: Scope): Checked[PlannedNoticeKey] =
    for
      noticeKey <- expectOrderToNoticeKey.evalAsString(scope)
      plannedNoticeKey <- PlannedNoticeKey.global(noticeKey)
    yield
      plannedNoticeKey


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
      postOrderToNoticeKey = expr(orderToNoticeId),
      expectOrderToNoticeKey = expr(orderToNoticeId),
      endOfLife = expr("$js7EpochMilli + " + lifetime.toMillis))

  private val sosOrderToNotice =
    expr("""match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$1')""")

  /** A GlobalBoard for JOC-style OrderIds. */
  def joc(boardPath: BoardPath, lifetime: Option[FiniteDuration] = Some(24.h)): GlobalBoard =
    GlobalBoard(
      boardPath,
      postOrderToNoticeKey = sosOrderToNotice,
      expectOrderToNoticeKey = sosOrderToNotice,
      endOfLife = lifetime.fold(MissingConstant):
        lifetime => expr("$js7EpochMilli + " + lifetime.toMillis))

