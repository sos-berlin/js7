package js7.data.board

import io.circe.generic.extras.Configuration.default.withDefaults
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.item.{ItemRevision, UnsignedSimpleItem}
import js7.data.value.expression.{Expression, Scope}

final case class Board(
  path: BoardPath,
  toNotice: Expression,
  expectingOrderToNoticeId: Expression,
  endOfLife: Expression,
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem
{
  protected type Self = Board
  val companion = Board

  def withRevision(revision: Option[ItemRevision]) =
    copy(itemRevision = revision)

  def postingOrderToNotice(scope: Scope): Checked[Notice] = {
    //val args = ListValue(Vector(StringValue(order.id.string)))
    for {
      endOfLife <- endOfLife
        .eval(scope)
        .flatMap(_.asLong)
        .map(Timestamp.ofEpochMilli)
      notice <- toNotice
        .eval/*(args)*/(scope)
        .flatMap(Notice.fromValue(_, endOfLife))
    } yield notice
  }

  def expectingOrderToNoticeId(scope: Scope): Checked[NoticeId] =
    expectingOrderToNoticeId
      .eval(scope)
      .flatMap(_.asString)
      .flatMap(NoticeId.checked)
}

object Board extends UnsignedSimpleItem.Companion[Board]
{
  val cls = classOf[Board]

  type Key = BoardPath
  val Key = BoardPath

  implicit val jsonCodec = {
    intelliJuseImport(FiniteDurationJsonEncoder)
    implicit val configuration = withDefaults
    deriveConfiguredCodec[Board]
  }
}
