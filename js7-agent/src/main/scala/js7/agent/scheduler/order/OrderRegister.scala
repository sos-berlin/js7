package js7.agent.scheduler.order

import js7.agent.scheduler.order.OrderRegister.*
import js7.base.monixlike.SerialSyncCancelable
import js7.core.common.ActorRegister
import js7.data.order.OrderId
import org.apache.pekko.actor.ActorRef
import scala.concurrent.Promise

/**
  * @author Joacim Zschimmer
  */
private[order] final class OrderRegister extends ActorRegister[OrderId, OrderEntry](_.actor):

  def recover(orderId: OrderId, actor: ActorRef): OrderEntry =
    val orderEntry = new OrderEntry(orderId, actor)
    insert(orderId -> orderEntry)
    orderEntry

  def insert(orderId: OrderId, actor: ActorRef): Unit =
    insert(orderId -> new OrderEntry(orderId, actor))

  def onActorTerminated(actor: ActorRef): Unit =
    remove(actorToKey(actor))

  override def remove(orderId: OrderId): Option[OrderEntry] =
    for orderEntry <- super.remove(orderId) yield
      orderEntry.timer.cancel()
      orderEntry

private[order] object OrderRegister:
  final class OrderEntry(
    val orderId: OrderId,
    val actor: ActorRef):
    val timer = SerialSyncCancelable()
    var detachResponses: List[Promise[Unit]] = Nil

    def isDetaching = detachResponses.nonEmpty
