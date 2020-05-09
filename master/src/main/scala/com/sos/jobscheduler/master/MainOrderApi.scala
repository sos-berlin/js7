package com.sos.jobscheduler.master

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrderId}
import com.sos.jobscheduler.master.data.MasterState
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
