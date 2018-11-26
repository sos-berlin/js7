package com.sos.jobscheduler.core.workflow

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.core.workflow.OrderEventHandler.FollowUp
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.OrderActorEvent
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowId}
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class OrderProcessor(
  idToWorkflow: WorkflowId ⇒ Checked[Workflow],
  idToOrder: PartialFunction[OrderId, Order[Order.State]])  // TODO OrderId => Checked[Order]
{
  private val eventHandler = new OrderEventHandler(idToWorkflow, idToOrder)
  private val eventSource = new OrderEventSource(idToWorkflow, idToOrder)

  def nextEvent(orderId: OrderId): Checked[Option[KeyedEvent[OrderActorEvent]]] =
    eventSource.nextEvent(orderId) mapProblem (_ withPrefix s"Problem with '$orderId':")

  def cancel(orderId: OrderId, isAgent: Boolean): Checked[Option[OrderActorEvent]] =
    eventSource.cancel(orderId, isAgent = isAgent)

  def handleEvent(keyedEvent: KeyedEvent[OrderEvent]): Checked[Seq[FollowUp]] =
    eventHandler.handleEvent(keyedEvent) mapProblem (_ withPrefix s"Problem with event $keyedEvent:")

  def offeredToAwaitingOrder(orderId: OrderId): Set[OrderId] =
    eventHandler.offeredToAwaitingOrder(orderId)
}
