package js7.data.board

import js7.base.utils.ScalaUtils.orderingBy
import js7.data.plan.{PlanId, PlanKey, PlanSchemaId}
import org.jetbrains.annotations.TestOnly

/** A PlannedBoardId denotes a PlannabeBoard or a GlobalBoard.
  */
final case class PlannedBoardId(planId: PlanId, boardPath: BoardPath):
  export planId.{planSchemaId, planKey}

  def /(noticeKey: NoticeKey): NoticeId =
    NoticeId(planId, boardPath / noticeKey)

  @TestOnly
  def /(noticeKey: String): NoticeId =
    this / NoticeKey(noticeKey)

  override def toString = s"PlannedBoard:${planId.shortString}╱${boardPath.string}"


object PlannedBoardId:
  given Ordering[PlannedBoardId] = orderingBy(_.planId, _.boardPath)
