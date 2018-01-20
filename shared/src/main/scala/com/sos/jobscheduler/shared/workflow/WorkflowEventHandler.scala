package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.{OrderForked, OrderJoined}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.shared.workflow.WorkflowEventHandler._
import com.sos.jobscheduler.shared.workflow.WorkflowProcessor.FollowUp
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class WorkflowEventHandler(idToOrder: PartialFunction[OrderId, Order[Order.State]])
{
  def handleEvent(keyedEvent: KeyedEvent[OrderEvent]): Seq[FollowUp] = {
    val KeyedEvent(orderId, event) = keyedEvent
    val previousOrder = idToOrder(orderId)
    event match {
      case event: OrderForked ⇒
        previousOrder.newForkedOrders(event) map FollowUp.Add.apply

      case joined: OrderJoined ⇒
        previousOrder.state match {
          case Order.Join(joinOrderIds) ⇒
            joinOrderIds map FollowUp.Remove.apply
          case state ⇒
            logger.error(s"Event $joined, but Order is in state $state")
            Nil
        }

      case _ ⇒
        Nil
    }
  }
}

object WorkflowEventHandler {
  private val logger = Logger(getClass)
}
