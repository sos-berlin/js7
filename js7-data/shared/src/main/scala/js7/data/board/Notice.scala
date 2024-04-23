package js7.data.board

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.time.Timestamp
import js7.data.order.OrderEvent.OrderNoticesExpected

final case class Notice(id: NoticeId, boardPath: BoardPath, endOfLife: Timestamp)
extends BoardSnapshot:
  def toExpected: OrderNoticesExpected.Expected =
    OrderNoticesExpected.Expected(boardPath, id)


object Notice:
  implicit val jsonCodec: Codec.AsObject[Notice] = deriveCodec
