package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAwaiting, OrderForked, OrderJoined, OrderOffered}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.shared.workflow.OrderEventHandler._
import scala.collection.immutable.Seq
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class OrderEventHandler(
  idToOrder: PartialFunction[OrderId, Order[Order.State]])
{
  private val _offeredToAwaitingOrder = mutable.Map[OrderId, Set[OrderId]]()

  def offeredToAwaitingOrder(orderId: OrderId): Set[OrderId] =
    _offeredToAwaitingOrder.getOrElse(orderId, Set.empty)

  def handleEvent(keyedEvent: KeyedEvent[OrderEvent]): Seq[FollowUp] = {
    val KeyedEvent(orderId, event) = keyedEvent
    val previousOrder = idToOrder(orderId)
    event match {
      case event: OrderForked ⇒
        previousOrder.newForkedOrders(event) map FollowUp.AddChild.apply

      case joined: OrderJoined ⇒
        previousOrder.state match {
          case Order.Join(joinOrderIds) ⇒
            joinOrderIds map FollowUp.Remove.apply

          case Order.Awaiting(_) ⇒
            _offeredToAwaitingOrder -= idToOrder(orderId).castState[Order.Awaiting].state.offeredOrderId
            Nil  // Offered order is being kept

          case state ⇒
            logger.error(s"Event $joined, but Order is in state $state")
            Nil
        }

      case event: OrderOffered ⇒
        FollowUp.AddOffered(previousOrder.newPublishedOrder(event)) :: Nil

      case OrderAwaiting(offeredOrderId) ⇒
        _offeredToAwaitingOrder(offeredOrderId) = _offeredToAwaitingOrder.getOrElse(offeredOrderId, Set.empty) + orderId
        Nil

      case _ ⇒
        Nil
    }
  }
}

object OrderEventHandler {
  private val logger = Logger(getClass)

  sealed trait FollowUp
  object FollowUp {
    final case class AddChild(order: Order[Order.Ready.type]) extends FollowUp
    final case class AddOffered(order: Order[Order.Offered]) extends FollowUp
    final case class TryAgain(orderId: OrderId) extends FollowUp
    final case class Remove(orderId: OrderId) extends FollowUp
  }
}
