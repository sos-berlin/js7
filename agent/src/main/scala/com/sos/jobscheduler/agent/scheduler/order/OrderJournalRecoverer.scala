package com.sos.jobscheduler.agent.scheduler.order

import akka.util.Timeout
import com.sos.jobscheduler.agent.AgentState
import com.sos.jobscheduler.agent.data.event.AgentMasterEvent
import com.sos.jobscheduler.base.utils.Collections.implicits.{InsertableMutableMap, RichTraversable}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichPartialFunction
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.JournalRecoverer
import com.sos.jobscheduler.core.workflow.Recovering.followUpRecoveredSnapshots
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderCoreEvent, OrderForked, OrderJoined, OrderStdWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowEvent}
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
private[order] final class OrderJournalRecoverer(protected val journalMeta: JournalMeta[Event])
(implicit askTimeout: Timeout)
extends JournalRecoverer[Event] {

  private val workflowRegister = new WorkflowRegister
  private val idToOrder = mutable.Map[OrderId, Order[Order.State]]()

  protected def recoverSnapshot = {
    case workflow: Workflow ⇒
      workflowRegister.recover(workflow)

    case order: Order[Order.State] ⇒
      idToOrder.insert(order.id → order)
  }

  override protected def onAllSnapshotRecovered() = {
    val (added, removed) = followUpRecoveredSnapshots(workflowRegister.idToWorkflow.checked, idToOrder.toMap)
    idToOrder ++= added.map(o ⇒ o.id → o)
    idToOrder --= removed
  }

  protected def recoverEvent = {
    case Stamped(_, _, KeyedEvent(_: NoKey, event: WorkflowEvent.WorkflowAttached)) ⇒
      workflowRegister.handleEvent(NoKey <-: event)

    case Stamped(_, _, KeyedEvent(orderId: OrderId, event: OrderEvent)) ⇒
      handleEvent(orderId, event)

    case Stamped(_, _, KeyedEvent(_, _: AgentMasterEvent.AgentReadyForMaster)) ⇒
  }

  private def handleEvent(orderId: OrderId, event: OrderEvent) =
    event match {
      case event: OrderEvent.OrderAttached ⇒
        idToOrder.insert(orderId → Order.fromOrderAttached(orderId, event))

      case OrderEvent.OrderDetached ⇒
        idToOrder -= orderId

      case event: OrderEvent ⇒
        // See also OrderActor#update
        event match {
          case event: OrderCoreEvent ⇒
            handleForkJoinEvent(orderId, event)
            idToOrder(orderId) = idToOrder(orderId).forceUpdate(event)
          case _: OrderStdWritten ⇒
            // OrderStdWritten is not handled (but forwarded to Master)
        }
    }

  private def handleForkJoinEvent(orderId: OrderId, event: OrderCoreEvent): Unit =  // TODO Duplicate with MasterJournalRecoverer
    event match {
      case event: OrderForked ⇒
        for (childOrder ← idToOrder(orderId).newForkedOrders(event)) {
          idToOrder.insert(childOrder.id → childOrder)
        }

      case event: OrderJoined ⇒
        idToOrder(orderId).state match {
          case forked: Order.Forked ⇒
            idToOrder --= forked.childOrderIds

          case state ⇒
            sys.error(s"Event $event recovered, but $orderId is in state $state")
        }

      case _ ⇒
    }

  def state = AgentState(lastRecoveredEventId, idToOrder.toMap, workflowRegister.workflows toKeyedMap (_.id))
}
