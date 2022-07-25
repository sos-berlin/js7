package js7.data.board

import io.circe.Codec
import js7.base.circeutils.CirceUtils.{DecodeWithDefaults, deriveConfiguredCodec}
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.item.{ItemRevision, UnsignedSimpleItem}
import js7.data.value.expression.{Expression, Scope}

final case class Board(
  path: BoardPath,
  postOrderToNoticeId: Expression,
  expectOrderToNoticeId: Expression,
  endOfLife: Expression,
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem
{
  protected type Self = Board
  val companion = Board

  def withRevision(revision: Option[ItemRevision]) =
    copy(itemRevision = revision)

  def rename(path: BoardPath) =
    copy(path = path)

  def toInitialItemState = BoardState(this)

  def postingOrderToNotice(scope: Scope): Checked[Notice] =
    for {
      value <- postOrderToNoticeId.eval(scope)
      string <- value.asString
      notice <- toNotice(NoticeId(string))(scope)
    } yield notice

  private def evalEndOfLife(scope: Scope): Checked[Timestamp] =
    endOfLife
      .eval(scope)
      .flatMap(_.asLongIgnoreFraction)
      .map(Timestamp.ofEpochMilli)

  def expectingOrderToNoticeId(scope: Scope): Checked[NoticeId] =
    for {
      value <- expectOrderToNoticeId.eval(scope)
      string <- value.asString
      noticeId <- NoticeId.checked(string)
    } yield noticeId

  def toNotice(noticeId: NoticeId, endOfLife: Option[Timestamp] = None)(scope: Scope)
  : Checked[Notice] =
    for (endOfLife <- endOfLife.fold(evalEndOfLife(scope))(Checked(_)))
      yield Notice(noticeId, path, endOfLife)
}

object Board extends UnsignedSimpleItem.Companion[Board]
{
  val cls = classOf[Board]

  type Key = BoardPath
  val Key = BoardPath

  override type Path = BoardPath
  override val Path = BoardPath

  type ItemState = BoardState

  implicit val jsonCodec: Codec.AsObject[Board] = deriveConfiguredCodec[Board]

  intelliJuseImport((FiniteDurationJsonEncoder, DecodeWithDefaults))
}
