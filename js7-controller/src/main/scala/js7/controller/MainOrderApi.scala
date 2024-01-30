package js7.controller

import cats.effect.IO
import js7.base.problem.Checked
import js7.data.controller.ControllerState
import js7.data.order.{Order, OrderId}

private[controller] class MainOrderApi(controllerState: IO[Checked[ControllerState]])
extends OrderApi:

  def order(orderId: OrderId): IO[Checked[Option[Order[Order.State]]]] =
    controllerState.map(_.map(_.idToOrder.get(orderId)))

  def orders: IO[Checked[Iterable[Order[Order.State]]]] =
    controllerState.map(_.map(_.idToOrder.values))

  override def orderIds: IO[Checked[Iterable[OrderId]]] =
    controllerState.map(_.map(_.idToOrder.keys))

  def orderCount =
    controllerState.map(_.map(_.idToOrder.size))
