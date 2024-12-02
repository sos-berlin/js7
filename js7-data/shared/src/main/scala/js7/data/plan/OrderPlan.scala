package js7.data.plan

import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.OrderId

final case class OrderPlan(planId: PlanId, orderIds: Set[OrderId]):

  override def toString =
    s"Plan($planId ${orderIds.toVector.sorted.mkString(" ")})"
      //s" ${plannedBoardPaths.toVector.sorted.mkString(" ")})"

  def isEmpty: Boolean =
    orderIds.isEmpty //&& plannedBoardPaths.isEmpty

  def hasOrders: Boolean =
    orderIds.nonEmpty

  def addOrders(orderIds: IterableOnce[OrderId]): OrderPlan =
    copy(orderIds = this.orderIds ++ orderIds)

  def removeOrders(orderIds: IterableOnce[OrderId]): OrderPlan =
    copy(orderIds = this.orderIds -- orderIds)


object OrderPlan:

  def initial(planId: PlanId): OrderPlan =
    OrderPlan(planId, Set.empty/*, Set.empty*/)

  given Ordering[OrderPlan] = Ordering.by(_.planId)
