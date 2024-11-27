package js7.data.controller

import js7.base.problem.Checked.RichCheckedIterable
import js7.base.problem.{Checked, Problem}
import js7.data.event.Event
import js7.data.order.OrderEvent.OrderAddedX
import js7.data.order.{MinimumOrder, Order, OrderEvent, OrderId}
import js7.data.plan.{PlanId, PlanItem}
import js7.data.state.EventDrivenStateView

trait ControllerStateView[Self <: ControllerStateView[Self]]
extends EventDrivenStateView[Self, Event]:
  this: Self =>

  protected def addOrder(order: Order[Order.State]): Checked[Self]

  override protected def applyOrderEvent(orderId: OrderId, event: OrderEvent): Checked[Self] =
    event match
      case orderAdded: OrderAddedX =>
          addOrder:
            Order.fromOrderAdded(
              orderAdded.ownOrderId getOrElse orderId,
              orderAdded)

      case _ =>
        super.applyOrderEvent(orderId, event)

  final def minimumOrderToPlanId(order: MinimumOrder): Checked[Option[PlanId]] =
    val scope = toMinimumOrderScope(order)
    keyToItem(PlanItem).values.flatMap:
      _.evalOrderToPlanId(scope)
    .combineProblems
    .flatMap: planIds =>
      planIds.length match
        case 0 => Right(None)
        case 1 => Right(Some(planIds.head))
        case n => Left(Problem:
          s"${order.id} fits $n Plans: ${planIds.mkString(", ")} — An Order must not fit multiple Plans")