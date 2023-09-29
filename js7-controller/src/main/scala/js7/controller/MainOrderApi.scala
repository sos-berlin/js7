package js7.controller

import js7.base.problem.Checked
import js7.data.controller.ControllerState
import js7.data.order.{Order, OrderId}
import monix.eval.Task

private[controller] class MainOrderApi(controllerState: Task[Checked[ControllerState]])
extends OrderApi:
  def order(orderId: OrderId): Task[Checked[Option[Order[Order.State]]]] =
    controllerState.map(_.map(_.idToOrder.get(orderId)))

  def orders: Task[Checked[Iterable[Order[Order.State]]]] =
    controllerState.map(_.map(_.idToOrder.values))

  override def orderIds: Task[Checked[Iterable[OrderId]]] =
    controllerState.map(_.map(_.idToOrder.keys))

  def orderCount =
    controllerState.map(_.map(_.idToOrder.size))
