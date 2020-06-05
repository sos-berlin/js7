package js7.master

import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.data.order.{FreshOrder, Order, OrderId, OrdersOverview}
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait OrderApi
{
  def order(orderId: OrderId): Task[Checked[Option[Order[Order.State]]]]

  def orders: Task[Checked[Iterable[Order[Order.State]]]]

  def orderIds: Task[Checked[Iterable[OrderId]]] =
    orders.map(_.map(_.map(_.id)))

  def ordersOverview: Task[Checked[OrdersOverview]] =
    orderCount.map(_.map(OrdersOverview.apply))

  def orderCount: Task[Checked[Int]]
}

object OrderApi
{
  trait WithCommands extends OrderApi {
    def addOrder(order: FreshOrder): Task[Checked[Boolean]]

    def addOrders(orders: Seq[FreshOrder]): Task[Checked[Completed]]
  }
}
