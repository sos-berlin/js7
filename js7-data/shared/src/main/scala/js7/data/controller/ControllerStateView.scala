package js7.data.controller

import js7.base.problem.Checked
import js7.data.event.Event
import js7.data.order.OrderEvent.{OrderAdded, OrderOrderAdded}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.state.EventDrivenStateView

trait ControllerStateView[Self <: ControllerStateView[Self]]
extends EventDrivenStateView[Self, Event]:
  this: Self =>

  protected def addOrder(order: Order[Order.State]): Checked[Self]

  override protected def applyOrderEvent(orderId: OrderId, event: OrderEvent): Checked[Self] =
    event match
      case orderAdded: OrderAdded =>
        addOrder(Order.fromOrderAdded(orderId, orderAdded))

      case orderAdded: OrderOrderAdded =>
        addOrder(Order.fromOrderAdded(orderAdded.orderId, orderAdded))

      case _ =>
        super.applyOrderEvent(orderId, event)
