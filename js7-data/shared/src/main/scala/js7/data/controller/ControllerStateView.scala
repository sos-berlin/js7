package js7.data.controller

import js7.base.problem.Checked.RichCheckedIterable
import js7.base.problem.{Checked, Problem}
import js7.data.event.Event
import js7.data.order.OrderEvent.OrderAddedX
import js7.data.order.{Order, OrderDetails, OrderEvent, OrderId}
import js7.data.plan.{PlanId, PlanItem}
import js7.data.state.EventDrivenStateView

trait ControllerStateView[Self <: ControllerStateView[Self]]
extends EventDrivenStateView[Self, Event]:
  this: Self =>

  protected def addOrder(order: Order[Order.State]): Checked[Self]

  override protected def applyOrderEvent(orderId: OrderId, event: OrderEvent): Checked[Self] =
    event match
      case orderAdded: OrderAddedX =>
        orderToPlanId(orderId, orderAdded).flatMap: planId =>
          addOrder:
            Order.fromOrderAdded(
              orderAdded.ownOrderId getOrElse orderId,
              orderAdded,
              planId.maybe)

      case _ =>
        super.applyOrderEvent(orderId, event)

  final def orderToPlanId(orderId: OrderId, order: OrderDetails): Checked[PlanId] =
    val scope = toMinimumOrderScope(orderId, order)
    keyToItem(PlanItem).values.flatMap:
      _.evalOrderToPlanId(scope)
    .combineProblems
    .flatMap: planIds =>
      planIds.length match
        case 0 => Right(PlanId.Global)
        case 1 => Right(planIds.head)
        case _ => Left(Problem:
          s"Multiple Plans match $orderId: ${planIds.map(_.planItemId).mkString(" ")}")
