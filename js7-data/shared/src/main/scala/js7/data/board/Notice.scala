package js7.data.board

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.time.Timestamp
import js7.base.utils.Big
import js7.data.order.OrderId

/** A NoticePlace may contain both a notice and an expectation,
 * when an order awaits notices from multiple Boards.
 */
final case class NoticePlace(
  notice: Option[Notice] = None,
  expectation: Option[NoticeExpectation] = None)
{
  assert(notice.isDefined | expectation.isDefined)

  def id: NoticeId = notice.fold(expectation.get.id)(_.id)
}
object NoticePlace
{
  implicit val jsonCodec = deriveCodec[NoticePlace]
}

final case class Notice(id: NoticeId, boardPath: BoardPath, endOfLife: Timestamp)
object Notice
{
  implicit val jsonCodec: Codec.AsObject[Notice] = deriveCodec[Notice]
}

final case class NoticeExpectation(id: NoticeId, orderIds: Set[OrderId])
extends Big
{
  def isEmpty = orderIds.isEmpty
}
object NoticeExpectation
{
  implicit val jsonCodec: Codec.AsObject[NoticeExpectation] = deriveCodec[NoticeExpectation]  // TODO Big
}
