package js7.data.board

import io.circe.generic.semiauto.deriveCodec
import js7.base.time.Timestamp
import js7.base.utils.Big
import js7.data.order.OrderId

sealed trait NoticePlace
{
  def id: NoticeId
}

final case class Notice(id: NoticeId, boardPath: BoardPath, endOfLife: Timestamp)
extends NoticePlace

object Notice
{
  implicit val jsonCodec = deriveCodec[Notice]
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
