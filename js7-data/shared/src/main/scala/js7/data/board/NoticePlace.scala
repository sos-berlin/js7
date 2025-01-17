package js7.data.board

import js7.base.circeutils.CirceUtils.deriveCodecWithDefaults
import js7.base.circeutils.typed.Subtype
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Big
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.NoticePlace.*
import js7.data.order.OrderId
import js7.data.plan.PlanId

/** A NoticePlace may contain both a notice and an expectation,
  * when an order awaits notices from multiple Boards.
  *
  * @param isAnnounced only for PlannedBoard
  */
final case class NoticePlace(
  noticeId: NoticeId,
  notice: Option[Notice] = None,
  expectingOrderIds: Set[OrderId] = Set.empty,
  isAnnounced: Boolean = false,
  isInConsumption: Boolean = false,
  consumptionCount: Int = 0)
extends Big:

  override def toString =
    s"NoticePlace(${notice getOrElse noticeId
    }${isAnnounced ?? " isAnnounced"
    }${isInConsumption ?? " isInConsumption"
    }${(consumptionCount != 0) ?? s" consumptionCount=$consumptionCount"
    }${expectingOrderIds.nonEmpty ?? s" expectingOrderIds=${expectingOrderIds.toVector.sorted.mkString(" ")}"})"

  def checked: Checked[this.type] =
    (notice.forall(_.id == noticeId) !!
      Problem(s"NoticePlace($noticeId) with different NoticeIds")
    ).rightAs(this)

  def isEmpty: Boolean =
    notice.isEmpty &&
      expectingOrderIds.isEmpty &&
      !isAnnounced &&
      !isInConsumption &&
      consumptionCount == 0

  def isInUse: Boolean =
    expectingOrderIds.nonEmpty || isInConsumption || consumptionCount != 0

  def toSnapshot(boardPath: BoardPath): Option[Snapshot] =
    (isAnnounced || isInConsumption || consumptionCount != 0) ?
      Snapshot(boardPath, noticeId, isAnnounced, isInConsumption, consumptionCount)

  def withSnapshot(snapshot: Snapshot): NoticePlace =
    copy(
      isAnnounced = snapshot.isAnnounced,
      isInConsumption = snapshot.isInConsumption,
      consumptionCount = snapshot.consumptionCount)

  def announce: NoticePlace =
    copy(isAnnounced = true)

  def post(notice: Notice): NoticePlace =
    copy(
      notice = Some(notice),
      isAnnounced = false,
      isInConsumption = false)

  def removeNotice: NoticePlace =
    copy(notice = None)

  def addExpecting(orderId: OrderId): NoticePlace =
    copy(expectingOrderIds = expectingOrderIds + orderId)

  def removeExpecting(orderId: OrderId): NoticePlace =
    copy(expectingOrderIds = expectingOrderIds - orderId)

  def startConsumption(orderId: OrderId): NoticePlace =
    copy(
      expectingOrderIds = expectingOrderIds - orderId,
      isInConsumption = true,
      consumptionCount = consumptionCount + 1)

  def finishConsumption(succeeded: Boolean): NoticePlace =
    val isLast = consumptionCount == 1
    copy(
      notice =
        if succeeded && isInConsumption && isLast then
          None
        else
          notice,
      //post already did this: isAnnounced = isAnnounced && !isLast,
      isInConsumption = !isLast,
      consumptionCount = consumptionCount - 1)

  def noticeKey: NoticeKey =
    noticeId.noticeKey

  def planId: PlanId =
    noticeId.planId


object NoticePlace:

  given Ordering[NoticePlace] = Ordering.by(_.noticeId)

  private[board] final case class Snapshot(
    boardPath: BoardPath,
    noticeId: NoticeId,
    isAnnounced: Boolean = false,
    isInConsumption: Boolean = false,
    consumptionCount: Int = 0)
  extends NoticeSnapshot:
    override def productPrefix = "NoticePlace.Snapshot"

  object Snapshot:
    val subtype: Subtype[Snapshot] =
      Subtype.named(deriveCodecWithDefaults[Snapshot], "NoticePlace")
