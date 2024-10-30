package js7.data.board

import io.circe.Codec
import io.circe.derivation.ConfiguredCodec
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.circeutils.typed.Subtype
import js7.base.problem.Checked
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.time.Timestamp
import js7.data.item.{ItemRevision, UnsignedItemPath, UnsignedSimpleItem, UnsignedSimpleItemPath}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.expression.{Expression, Scope}
import scala.concurrent.duration.FiniteDuration

final case class GlobalBoard(
  path: BoardPath,
  postOrderToNoticeId: Expression,
  expectOrderToNoticeId: Expression,
  endOfLife: Expression,
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem:

  protected type Self = GlobalBoard
  val companion: GlobalBoard.type = GlobalBoard

  def withRevision(revision: Option[ItemRevision]): GlobalBoard =
    copy(itemRevision = revision)

  def rename(path: BoardPath): GlobalBoard =
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


object GlobalBoard extends UnsignedSimpleItem.Companion[GlobalBoard]:
  val cls: Class[GlobalBoard] = classOf[GlobalBoard]

  type Key = BoardPath
  def Key: UnsignedSimpleItemPath.Companion[BoardPath] = BoardPath

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
    expr("""replaceAll($js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', '$1')""")

  /** A GlobalBoard for JOC-style OrderIds. */
  def joc(boardPath: BoardPath, lifetime: FiniteDuration = 24.h): GlobalBoard =
    GlobalBoard(
      boardPath,
      postOrderToNoticeId = sosOrderToNotice,
      expectOrderToNoticeId = sosOrderToNotice,
      endOfLife = expr("$js7EpochMilli + " + lifetime.toMillis))

  given jsonCodec: Codec.AsObject[GlobalBoard] = ConfiguredCodec.derive(useDefaults = true)
