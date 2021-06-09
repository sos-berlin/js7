package js7.data.execution.workflow

import js7.base.problem.Checked
import js7.base.problem.Checked._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.OrderEventHandler.FollowUp
import js7.data.order.OrderEvent.{OrderDeleted, OrderForked}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.workflow.{Workflow, WorkflowId}
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
object WorkflowAndOrderRecovering
{
  // TODO Some events (OrderForked, OrderFinished, OrderOffered) handled by OrderActor let AgentOrderKeeper add or delete orders. This should be done in a single transaction.
  /** A snapshot of a freshly forked Order may contain the child orders. This is handled here. **/
  final def followUpRecoveredWorkflowsAndOrders(idToWorkflow: WorkflowId => Checked[Workflow], idToOrder: Map[OrderId, Order[Order.State]])
  : (Map[OrderId, Order[Order.State]], Set[OrderId]) = {
    val added = Map.newBuilder[OrderId, Order[Order.State]]
    val deleted = mutable.Buffer.empty[OrderId]
    val eventHandler = new OrderEventHandler(idToWorkflow, idToOrder.checked)
    for (order <- idToOrder.values;
         event <- snapshotToEvent(order);
         followUps <- eventHandler.handleEvent(event)
           .onProblem(p => scribe.error(p.toString, p.throwableOption.map(_.appendCurrentStackTrace).orNull)))  // TODO Really ignore error ?
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
    (added.result(), deleted.toSet)
  }

  private def snapshotToEvent(order: Order[Order.State]): Option[KeyedEvent[OrderEvent]] =
    order.ifState[Order.Forked].map(order =>
      order.id <-: OrderForked(order.state.children))
    //.orElse(
    //  order.ifState[Order.Offering].map(order =>
    //    Right(FollowUp.AddOffered(order.newOfferedOrder(event)) :: Nil)))
    //.orElse(
    //  order.ifState[Order.Awaiting].map(order =>   TODO Missing?
    //  )
    .orElse(
      order.ifState[Order.Deleted].map(_ =>
        order.id <-: OrderDeleted))
}
