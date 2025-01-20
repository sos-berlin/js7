package js7.data.board

import js7.base.utils.ScalaUtils.orderingBy
import js7.data.plan.PlanId
import org.jetbrains.annotations.TestOnly

final case class PlannedBoardId(planId: PlanId, boardPath: BoardPath):

  @TestOnly
  def /(noticeKey: NoticeKey): NoticeId =
    NoticeId(planId, boardPath, noticeKey)

  @TestOnly
  def /(noticeKey: String): NoticeId =
    this / NoticeKey(noticeKey)

  override def toString = s"PlannedBoard:${planId.shortString}/${boardPath.string}"




object PlannedBoardId:
  given Ordering[PlannedBoardId] = orderingBy(_.planId, _.boardPath)
