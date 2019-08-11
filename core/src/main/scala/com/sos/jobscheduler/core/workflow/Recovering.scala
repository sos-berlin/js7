package com.sos.jobscheduler.core.workflow

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.Logger.ops.RichScalaLogger
import com.sos.jobscheduler.core.workflow.OrderEventHandler.FollowUp
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.{OrderFinished, OrderForked}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowId}
import scala.collection.immutable.Iterable
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
object Recovering
{
  private val logger = Logger(getClass)

  // TODO Some events (OrderForked, OrderFinished, OrderOffered) handels by OrderActor let AgentOrderKeeper add or remove orders. This should be done in a single transaction.
  /** A snapshot of a freshly forked Order may contain the child orders. This is handled here. **/
  final def followUpRecoveredSnapshots(idToWorkflow: WorkflowId => Checked[Workflow], idToOrder: Map[OrderId, Order[Order.State]])
  : (Iterable[Order[Order.State]], Iterable[OrderId]) = {
    val added = mutable.Map[OrderId, Order[Order.State]]()
    val removed = mutable.Buffer[OrderId]()
    val orderProcessor = new OrderProcessor(idToWorkflow, idToOrder)
    for (order <- idToOrder.values;
         event <- snapshotToEvent(order);
         followUps <- orderProcessor.handleEvent(event) onProblem (p => logger.error(p)))
    {
      followUps foreach {
        case FollowUp.AddChild(childOrder) =>  // OrderForked
          if (!idToOrder.contains(childOrder.id)) {  // Snapshot of child order is missing? Add the child now!
            added.insert(childOrder.id -> childOrder)
          }

        case FollowUp.Remove(removeOrderId) =>  // OrderFinished
          removed += removeOrderId

        case _ =>
      }
    }
    (added.values.toVector, removed.toVector)
  }

  private def snapshotToEvent(order: Order[Order.State]): Option[KeyedEvent[OrderEvent]] =
    order.ifState[Order.Forked].map(order =>
      order.id <-: OrderForked(order.state.children))
    //.orElse(
    //  order.ifState[Order.Offering].map(order =>
    //    Right(FollowUp.AddOffered(order.newPublishedOrder(event)) :: Nil)))
    //.orElse(
    //  order.ifState[Order.Awaiting].map(order =>   TODO Missing?
    //  )
    .orElse(
      order.ifState[Order.Finished].map(_ =>
        order.id <-: OrderFinished))
}
