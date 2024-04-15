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
  isInConsumption: Boolean = false,
  consumptionCount: Int = 0)
extends Big:

  def checked: Checked[this.type] =
    (notice.forall(_.id == noticeId) !!
      Problem(s"NoticePlace($noticeId) with different NoticeIds")
    ).rightAs(this)

  def isEmpty: Boolean =
    notice.isEmpty &&
      expectingOrderIds.isEmpty &&
      !isInConsumption &&
      consumptionCount == 0

  def toSnapshot(boardPath: BoardPath): Option[Snapshot] =
    (isInConsumption || consumptionCount != 0) ?
      Snapshot(boardPath, noticeId, isInConsumption, consumptionCount)

  def withSnapshot(snapshot: Snapshot): NoticePlace =
    copy(
      isInConsumption = snapshot.isInConsumption,
      consumptionCount = snapshot.consumptionCount)

  def post(notice: Notice): NoticePlace =
    copy(
      notice = Some(notice),
      isInConsumption = false)

  def removeNotice: NoticePlace =
    copy(notice = None)

  def startConsumption(orderId: OrderId): NoticePlace =
    copy(
      isInConsumption = true,
      consumptionCount = consumptionCount + 1,
      expectingOrderIds = expectingOrderIds - orderId)

  def finishConsumption(succeeded: Boolean): NoticePlace =
    val isLast = consumptionCount == 1
    copy(
      notice =
        if succeeded && isInConsumption && isLast then
          None
        else
          notice,
      isInConsumption = !isLast,
      consumptionCount = consumptionCount - 1)


object NoticePlace:
  final case class Snapshot(
    boardPath: BoardPath,
    noticeId: NoticeId,
    isInConsumption: Boolean,
    consumptionCount: Int)
  extends BoardSnapshot

  object Snapshot:
    val subtype: Subtype[Snapshot] =
      Subtype.named(deriveCodec[Snapshot], "NoticePlace")
