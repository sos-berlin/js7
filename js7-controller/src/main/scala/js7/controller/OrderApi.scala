package js7.controller

import cats.effect.IO
import js7.base.problem.Checked
import js7.data.order.{Order, OrderId, OrdersOverview}

/**
  * @author Joacim Zschimmer
  */
trait OrderApi:
  def order(orderId: OrderId): IO[Checked[Option[Order[Order.State]]]]

  def orders: IO[Checked[Iterable[Order[Order.State]]]]

  def orderIds: IO[Checked[Iterable[OrderId]]] =
    orders.map(_.map(_.map(_.id)))

  def ordersOverview: IO[Checked[OrdersOverview]] =
    orderCount.map(_.map(OrdersOverview.apply))

  def orderCount: IO[Checked[Int]]
