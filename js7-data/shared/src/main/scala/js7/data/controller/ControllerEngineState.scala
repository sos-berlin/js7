package js7.data.controller

import js7.base.problem.Checked
import js7.base.utils.L3
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.NoticeId
import js7.data.order.OrderEvent.OrderAddedX
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.plan.{PlanId, PlanSchemaState}
import js7.data.state.EngineState_

trait ControllerEngineState[Self <: ControllerEngineState[Self]]
extends EngineState_[Self]:
  this: Self =>

  def isNoticeAvailable(noticeId: NoticeId): L3

  override protected def applyOrderEvent(orderId: OrderId, event: OrderEvent): Checked[Self] =
    event match
      case orderAdded: OrderAddedX =>
        val addedOrderId = orderAdded.ownOrderId getOrElse orderId
        for
          _ <- idToOrder.checkNoDuplicate(addedOrderId)
          r <- addOrders(Order.fromOrderAdded(addedOrderId, orderAdded) :: Nil,
            allowClosedPlan = true/*the issuer of the event has already checked this*/)
        yield
          r

      case _ =>
        super.applyOrderEvent(orderId, event)

  def checkPlanIsOpen(planId: PlanId): Checked[Unit] =
    checkPlanAcceptsOrders(planId, allowClosedPlan = false)

  def checkPlanAcceptsOrders(planId: PlanId, allowClosedPlan: Boolean): Checked[Unit] =
    for
      planSchemaState <- keyTo(PlanSchemaState).checked(planId.planSchemaId)
      _ <- planSchemaState.checkPlanAcceptsOrders(planId.planKey, allowClosedPlan)
    yield
      ()
