package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.OrderActorEvent
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.shared.workflow.OrderEventHandler.FollowUp
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class OrderProcessor(
  pathToWorkflow: PartialFunction[WorkflowPath, Workflow],
  idToOrder: PartialFunction[OrderId, Order[Order.State]])
{
  private val eventHandler = new OrderEventHandler(idToOrder)
  private val eventSource = new OrderEventSource(pathToWorkflow, idToOrder)

  def nextEvent(orderId: OrderId): Option[KeyedEvent[OrderActorEvent]] =
    eventSource.nextEvent(orderId)

  def handleEvent(keyedEvent: KeyedEvent[OrderEvent]): Seq[FollowUp] =
    eventHandler.handleEvent(keyedEvent)

  def offeredToAwaitingOrder(orderId: OrderId): Set[OrderId] =
    eventHandler.offeredToAwaitingOrder(orderId)
}
