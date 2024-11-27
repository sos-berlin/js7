package js7.data.board

import io.circe.Codec
import io.circe.derivation.ConfiguredCodec
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.circeutils.typed.Subtype
import js7.base.problem.Checked
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.time.Timestamp
import js7.data.item.{ItemRevision, UnsignedItemPath}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.expression.{Expression, Scope}
import scala.concurrent.duration.FiniteDuration

final case class GlobalBoard(
  path: BoardPath,
  postOrderToNoticeId: Expression,
  expectOrderToNoticeId: Expression,
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

  def postingOrderToNotice(scope: Scope): Checked[Notice] =
    for
      noticeKey <- postOrderToNoticeId.evalAsString(scope)
      noticeId <- NoticeId.global(noticeKey)
      notice <- toNotice(noticeId)(scope)
    yield
      notice

  def toNotice(noticeId: NoticeId, endOfLife: Option[Timestamp] = None)(scope: Scope)
  : Checked[Notice] =
    for endOfLife <- endOfLife.fold(evalEndOfLife(scope))(Checked(_)) yield
      Notice(noticeId, path, Some(endOfLife))

  def expectingOrderToNoticeId(scope: Scope): Checked[NoticeId] =
    for
      noticeKey <- expectOrderToNoticeId.evalAsString(scope)
      noticeId <- NoticeId.global(noticeKey)
    yield
      noticeId

  private def evalEndOfLife(scope: Scope): Checked[Timestamp] =
    endOfLife
      .eval(scope)
      .flatMap(_.asLongIgnoreFraction)
      .map(Timestamp.ofEpochMilli)


object GlobalBoard extends BoardItem.Companion[GlobalBoard]:

  val cls: Class[GlobalBoard] = classOf[GlobalBoard]

  override type Path = BoardPath
  override val Path: UnsignedItemPath.Companion[BoardPath] = BoardPath

  type ItemState = BoardState

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
      postOrderToNoticeId = expr(orderToNoticeId),
      expectOrderToNoticeId = expr(orderToNoticeId),
      endOfLife = expr("$js7EpochMilli + " + lifetime.toMillis))

  private val sosOrderToNotice =
    expr("""match(orderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', '$1')""")

  /** A GlobalBoard for JOC-style OrderIds. */
  def joc(boardPath: BoardPath, lifetime: FiniteDuration = 24.h): GlobalBoard =
    GlobalBoard(
      boardPath,
      postOrderToNoticeId = sosOrderToNotice,
      expectOrderToNoticeId = sosOrderToNotice,
      endOfLife = expr("$js7EpochMilli + " + lifetime.toMillis))

  given jsonCodec: Codec.AsObject[GlobalBoard] = ConfiguredCodec.derive(useDefaults = true)
