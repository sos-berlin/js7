package js7.data.plan

import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.{BoardPath, NoticeKey}
import js7.data.order.OrderId

/** For now, this Plan is build for the user as required.
  * <p>
  *   For the internally used counterpart, see `OrderPlan` which omits the NoticeKeys.
  */
final case class Plan(
  planId: PlanId,
  orderIds: Set[OrderId],
  boardToNotices: Map[BoardPath, Set[NoticeKey]]):

  override def toString =
    s"Plan($planId ${orderIds.toVector.sorted.mkString(" ")} ${
      boardToNotices.toVector.sortBy(_._1).map: (boardPath, noticesKeys) =>
        s"$boardPath -> ${noticesKeys.toVector.sorted.mkString(" ")}"
      .mkString(", ")})"


object Plan:

  def initial(planId: PlanId): Plan =
    Plan(planId, Set.empty, Map.empty)

  def fromPlan(plan: OrderPlan): Plan =
    Plan(plan.planId, plan.orderIds, Map.empty)

  given Ordering[Plan] = Ordering.by(_.planId)
