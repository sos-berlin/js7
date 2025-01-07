package js7.data.board

import js7.base.utils.ScalaUtils.orderingBy
import js7.data.plan.PlanId

final case class PlannedBoardId(planId: PlanId, boardPath: BoardPath):
  override def toString = s"PlannedBoard:${planId.shortString}/${boardPath.string}"


object PlannedBoardId:
  given Ordering[PlannedBoardId] = orderingBy(_.planId, _.boardPath)
