package com.sos.jobscheduler.master

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.data.event.Stamped
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrderId}
import monix.eval.Task
import scala.collection.immutable.Seq
import shapeless.tag.@@

private[master] class MainOrderApi(orderKeeper: Task[ActorRef @@ MasterOrderKeeper])
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

  def order(orderId: OrderId): Task[Option[Order[Order.State]]] =
    orderKeeper.flatMap(actor =>
      Task.deferFuture(
        (actor ? MasterOrderKeeper.Command.GetOrder(orderId)).mapTo[Option[Order[Order.State]]]))

  def orders: Task[Stamped[Seq[Order[Order.State]]]] =
    orderKeeper.flatMap(actor =>
      Task.deferFuture(
        (actor ? MasterOrderKeeper.Command.GetOrders).mapTo[Stamped[Seq[Order[Order.State]]]]))

  def orderCount =
    orderKeeper.flatMap(actor =>
      Task.deferFuture(
        (actor ? MasterOrderKeeper.Command.GetOrderCount).mapTo[Int]))
}
