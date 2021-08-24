package js7.data.board

import io.circe.generic.semiauto.deriveCodec
import js7.base.time.Timestamp
import js7.base.utils.Big
import js7.data.order.OrderId

sealed trait NoticePlace
{
  def id: NoticeId
}

final case class Notice(id: NoticeId, endOfLife: Timestamp)
extends NoticePlace

object Notice
{
  final case class Snapshot(id: NoticeId, boardPath: BoardPath, endOfLife: Timestamp) {
    def notice = Notice(id, endOfLife)
  }

  object Snapshot {
    def apply(boardPath: BoardPath, notice: Notice) =
      new Snapshot(notice.id, boardPath, notice.endOfLife)
  }

  implicit val jsonCodec = deriveCodec[Notice]
  implicit val snapshotJsonCodec = deriveCodec[Notice.Snapshot]
}


final case class NoticeExpectation(id: NoticeId, orderIds: Set[OrderId])
extends NoticePlace with Big
{
  def isEmpty = orderIds.isEmpty
}

object NoticeExpectation
{
  implicit val jsonCodec = deriveCodec[NoticeExpectation]  // TODO Big
}
