package js7.data.board

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.Subtype
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Big
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.NoticePlace.*
import js7.data.order.OrderId

/** A NoticePlace may contain both a notice and an expectation,
 * when an order awaits notices from multiple Boards.
 */
final case class NoticePlace(
  noticeId: NoticeId,
  notice: Option[Notice] = None,
  expectingOrderIds: Set[OrderId] = Set.empty,
  noticeIsInConsumption: Boolean = false,
  consumptionCount: Int = 0)
extends Big
{
  def checked: Checked[this.type] =
    (notice.forall(_.id == noticeId) !!
      Problem(s"NoticePlace($noticeId) with different NoticeIds")
    ).rightAs(this)

  def isEmpty =
    notice.isEmpty &&
      expectingOrderIds.isEmpty &&
      !noticeIsInConsumption &&
      consumptionCount == 0

  def toSnapshot(boardPath: BoardPath): Option[Snapshot] =
    (noticeIsInConsumption || consumptionCount != 0) ?
      Snapshot(boardPath, noticeId, noticeIsInConsumption, consumptionCount)

  def withSnapshot(snapshot: Snapshot): NoticePlace =
    copy(
      noticeIsInConsumption = snapshot.noticeIsInConsumption,
      consumptionCount = snapshot.consumptionCount)

  def post(notice: Notice): NoticePlace =
    copy(
      notice = Some(notice),
      noticeIsInConsumption = false)

  def removeNotice: NoticePlace =
    copy(notice = None)

  def startConsuming: NoticePlace =
    copy(
      noticeIsInConsumption = true,
      consumptionCount = consumptionCount + 1)

  def finishConsuming(succeeded: Boolean): NoticePlace = {
    val isLast = consumptionCount == 1
    copy(
      notice =
        if (succeeded && noticeIsInConsumption && isLast)
          None
        else
          notice,
      noticeIsInConsumption = !isLast,
      consumptionCount = consumptionCount - 1)
  }
}

object NoticePlace
{
  final case class Snapshot(
    boardPath: BoardPath,
    noticeId: NoticeId,
    noticeIsInConsumption: Boolean,
    consumptionCount: Int)
  extends BoardSnapshot
  object Snapshot {
    val subtype = Subtype.named(deriveCodec[Snapshot], "NoticePlace")
  }
}
