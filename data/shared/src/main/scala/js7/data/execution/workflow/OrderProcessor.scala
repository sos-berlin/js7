package js7.data.execution.workflow

import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.data.command.CancelMode
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.OrderEventHandler.FollowUp
import js7.data.order.OrderEvent.OrderActorEvent
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.workflow.{Workflow, WorkflowId}

/**
  * @author Joacim Zschimmer
  */
final class OrderProcessor(
  idToWorkflow: WorkflowId => Checked[Workflow],
  idToOrder: PartialFunction[OrderId, Order[Order.State]])  // TODO OrderId => Checked[Order]
{
  private val eventHandler = new OrderEventHandler(idToWorkflow, idToOrder)
  private val eventSource = new OrderEventSource(idToWorkflow, idToOrder)

  def nextEvent(orderId: OrderId): Option[KeyedEvent[OrderActorEvent]] =
    eventSource.nextEvent(orderId)

  def cancel(orderId: OrderId, mode: CancelMode, isAgent: Boolean): Checked[Option[OrderActorEvent]] =
    eventSource.cancel(orderId, mode, isAgent = isAgent)

  def isOrderCancelable(order: Order[Order.State]): Boolean =
    eventSource.isOrderCancelable(order)

  def handleEvent(keyedEvent: KeyedEvent[OrderEvent]): Checked[Seq[FollowUp]] =
    eventHandler.handleEvent(keyedEvent) mapProblem (_ withPrefix s"Problem with event $keyedEvent:")

  def offeredToAwaitingOrder(orderId: OrderId): Set[OrderId] =
    eventHandler.offeredToAwaitingOrder(orderId)
}
