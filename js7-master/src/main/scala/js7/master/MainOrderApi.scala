package js7.master

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.data.order.{FreshOrder, Order, OrderId}
import js7.master.data.MasterState
import monix.eval.Task
import shapeless.tag.@@

private[master] class MainOrderApi(masterState: Task[Checked[MasterState]], orderKeeper: Task[ActorRef @@ MasterOrderKeeper])
  (implicit akkaAskTimeout: Timeout)
extends OrderApi.WithCommands
{
  def addOrder(order: FreshOrder) =
    orderKeeper.flatMap(actor =>
      Task.deferFuture(
        (actor ? MasterOrderKeeper.Command.AddOrder(order)).mapTo[MasterOrderKeeper.Response.ForAddOrder])
        .map(_.created))

  def addOrders(order: Seq[FreshOrder]) =
    orderKeeper.flatMap(actor =>
      Task.deferFuture(
        (actor ? MasterOrderKeeper.Command.AddOrders(order)).mapTo[Checked[Completed]]))

  def order(orderId: OrderId): Task[Checked[Option[Order[Order.State]]]] =
    masterState.map(_.map(_.idToOrder.get(orderId)))

  def orders: Task[Checked[Iterable[Order[Order.State]]]] =
    masterState.map(_.map(_.idToOrder.values))

  override def orderIds: Task[Checked[Iterable[OrderId]]] =
    masterState.map(_.map(_.idToOrder.keys))

  def orderCount =
    masterState.map(_.map(_.idToOrder.size))
}
