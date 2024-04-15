package js7.data.board

import io.circe.Codec
import io.circe.derivation.ConfiguredCodec
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.problem.Checked
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.time.Timestamp
import js7.data.item.{ItemRevision, UnsignedItemPath, UnsignedSimpleItem, UnsignedSimpleItemPath}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.expression.{Expression, Scope}
import scala.concurrent.duration.FiniteDuration

final case class Board(
  path: BoardPath,
  postOrderToNoticeId: Expression,
  expectOrderToNoticeId: Expression,
  endOfLife: Expression,
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem:

  protected type Self = Board
  val companion: Board.type = Board

  def withRevision(revision: Option[ItemRevision]): Board =
    copy(itemRevision = revision)

  def rename(path: BoardPath): Board =
    copy(path = path)

  def toInitialItemState: BoardState =
    BoardState(this)

  def postingOrderToNotice(scope: Scope): Checked[Notice] =
    for
      string <- postOrderToNoticeId.evalAsString(scope)
      notice <- toNotice(NoticeId(string))(scope)
    yield notice

  private def evalEndOfLife(scope: Scope): Checked[Timestamp] =
    endOfLife
      .eval(scope)
      .flatMap(_.asLongIgnoreFraction)
      .map(Timestamp.ofEpochMilli)

  def expectingOrderToNoticeId(scope: Scope): Checked[NoticeId] =
    for
      string <- expectOrderToNoticeId.evalAsString(scope)
      noticeId <- NoticeId.checked(string)
    yield noticeId

  def toNotice(noticeId: NoticeId, endOfLife: Option[Timestamp] = None)(scope: Scope)
  : Checked[Notice] =
    for endOfLife <- endOfLife.fold(evalEndOfLife(scope))(Checked(_))
      yield Notice(noticeId, path, endOfLife)


object Board extends UnsignedSimpleItem.Companion[Board]:
  val cls: Class[Board] = classOf[Board]

  type Key = BoardPath
  def Key: UnsignedSimpleItemPath.Companion[BoardPath] = BoardPath

  override type Path = BoardPath
  override val Path: UnsignedItemPath.Companion[BoardPath] = BoardPath

  type ItemState = BoardState

  /** A Board with a single, constant Notice. */
  def singleNotice(
    boardPath: BoardPath,
    orderToNoticeId: String = "'NOTICE'",
    lifetime: FiniteDuration = 24.h)
  : Board =
    Board(
      boardPath,
      postOrderToNoticeId = expr(orderToNoticeId),
      expectOrderToNoticeId = expr(orderToNoticeId),
      endOfLife = expr("$js7EpochMilli + " + lifetime.toMillis))

  private val sosOrderToNotice =
    expr("""replaceAll($js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', '$1')""")

  /** A Board for JOC-style OrderIds. */
  def joc(boardPath: BoardPath, lifetime: FiniteDuration = 24.h): Board =
    Board(
      boardPath,
      postOrderToNoticeId = sosOrderToNotice,
      expectOrderToNoticeId = sosOrderToNotice,
      endOfLife = expr("$js7EpochMilli + " + lifetime.toMillis))

  implicit val jsonCodec: Codec.AsObject[Board] = ConfiguredCodec.derive(useDefaults = true)
