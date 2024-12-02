package js7.data.controller

import js7.base.problem.Checked.RichCheckedIterable
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.Event
import js7.data.order.OrderEvent.OrderAddedX
import js7.data.order.{MinimumOrder, Order, OrderEvent, OrderId}
import js7.data.plan.{PlanId, PlanTemplate}
import js7.data.state.EventDrivenStateView

trait ControllerStateView[Self <: ControllerStateView[Self]]
extends EventDrivenStateView[Self, Event]:
  this: Self =>

  override protected def applyOrderEvent(orderId: OrderId, event: OrderEvent): Checked[Self] =
    event match
      case orderAdded: OrderAddedX =>
        val addedOrderId = orderAdded.ownOrderId getOrElse orderId
        idToOrder.checkNoDuplicate(addedOrderId).flatMap: _ =>
          update(addOrders =
            Order.fromOrderAdded(addedOrderId, orderAdded) :: Nil)

      case _ =>
        super.applyOrderEvent(orderId, event)

  final def minimumOrderToPlanId(order: MinimumOrder): Checked[Option[PlanId]] =
    val scope = toMinimumOrderScope(order)
    keyToItem(PlanTemplate).values.flatMap:
      _.evalOrderToPlanId(scope)
    .combineProblems
    .flatMap: planIds =>
      planIds.length match
        case 0 => Right(None)
        case 1 => Right(Some(planIds.head))
        case n => Left(Problem:
          s"${order.id} fits $n Plans: ${planIds.sorted.mkString(", ")
          } — An Order must not fit multiple Plans")
