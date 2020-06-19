package js7.controller

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.controller.data.ControllerState
import js7.data.order.{FreshOrder, Order, OrderId}
import monix.eval.Task
import shapeless.tag.@@

private[controller] class MainOrderApi(controllerState: Task[Checked[ControllerState]], orderKeeper: Task[ActorRef @@ ControllerOrderKeeper])
  (implicit akkaAskTimeout: Timeout)
extends OrderApi.WithCommands
{
  def addOrder(order: FreshOrder) =
    orderKeeper.flatMap(actor =>
      Task.deferFuture(
        (actor ? ControllerOrderKeeper.Command.AddOrder(order)).mapTo[ControllerOrderKeeper.Response.ForAddOrder])
        .map(_.created))

  def addOrders(order: Seq[FreshOrder]) =
    orderKeeper.flatMap(actor =>
      Task.deferFuture(
        (actor ? ControllerOrderKeeper.Command.AddOrders(order)).mapTo[Checked[Completed]]))

  def order(orderId: OrderId): Task[Checked[Option[Order[Order.State]]]] =
    controllerState.map(_.map(_.idToOrder.get(orderId)))

  def orders: Task[Checked[Iterable[Order[Order.State]]]] =
    controllerState.map(_.map(_.idToOrder.values))

  override def orderIds: Task[Checked[Iterable[OrderId]]] =
    controllerState.map(_.map(_.idToOrder.keys))

  def orderCount =
    controllerState.map(_.map(_.idToOrder.size))
}
