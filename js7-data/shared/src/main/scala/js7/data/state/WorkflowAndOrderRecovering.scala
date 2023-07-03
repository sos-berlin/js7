package js7.data.state

import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.data.order.OrderEvent.{OrderDeleted, OrderForked}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.state.OrderEventHandler.FollowUp
import js7.data.workflow.{Workflow, WorkflowId}

/**
  * @author Joacim Zschimmer
  */
object WorkflowAndOrderRecovering
{
  private val logger = Logger[this.type]

  // TODO Some events (OrderForked, OrderFinished) handled by OrderActor let AgentOrderKeeper add or delete orders. This should be done in a single transaction.
  /** A snapshot of a freshly forked Order may contain the child orders. This is handled here. **/
  final def followUpRecoveredWorkflowsAndOrders(
    idToWorkflow: WorkflowId => Checked[Workflow],
    idToOrder: Map[OrderId, Order[Order.State]])
  : (Map[OrderId, Order[Order.State]], Set[OrderId]) = {
    val added = Map.newBuilder[OrderId, Order[Order.State]]
    val deleted = Set.newBuilder[OrderId]
    val eventHandler = new OrderEventHandler(idToWorkflow)
    for (
      order <- idToOrder.values;
      event <- snapshotToEvent(order);
      followUps <- eventHandler.handleEvent(order, event)
        .onProblem(p =>
          logger.error(p.toString, p.throwableOption.map(_.appendCurrentStackTrace).orNull)))  // TODO Really ignore error ?
    {
      followUps foreach {
        case FollowUp.AddChild(childOrder) =>  // OrderForked
          if (!idToOrder.contains(childOrder.id)) {  // Snapshot of child order is missing? Add the child now!
            added += childOrder.id -> childOrder
          }

        case FollowUp.Delete(deleteOrderId) =>  // OrderDeleted, OrderJoined
          deleted += deleteOrderId

        case _ =>
      }
    }
    (added.result(), deleted.result())
  }

  private def snapshotToEvent(order: Order[Order.State]): Option[OrderEvent] =
    order.ifState[Order.Forked].map(order =>
      OrderForked(order.state.children))
    .orElse(
      order.ifState[Order.Deleted].map(_ =>
        OrderDeleted))
}
