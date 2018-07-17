package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.agent.scheduler.event.EventQueueActor
import com.sos.jobscheduler.base.utils.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.event.journal.{JournalMeta, JournalRecoverer}
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderCoreEvent, OrderDetached, OrderForked, OrderJoined, OrderStdWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowEvent}
import scala.collection.mutable
import shapeless.tag.@@

/**
  * @author Joacim Zschimmer
  */
private[order] final class OrderJournalRecoverer(protected val journalMeta: JournalMeta[Event], eventsForMaster: ActorRef @@ EventQueueActor)
(implicit askTimeout: Timeout)
extends JournalRecoverer[Event] {

  private val workflowRegister = new WorkflowRegister
  private val idToOrder = mutable.Map[OrderId, Order[Order.State]]()

  protected def recoverSnapshot = {
    case workflow: Workflow ⇒
      workflowRegister.recover(workflow)

    case order: Order[Order.State] ⇒
      idToOrder.insert(order.id → order)

    case snapshot: EventQueueActor.Snapshot ⇒
      eventsForMaster ! snapshot
  }

  protected def recoverEvent = {
    case Stamped(_, _, KeyedEvent(_: NoKey, event: WorkflowEvent.WorkflowAttached)) ⇒
      workflowRegister.handleEvent(NoKey <-: event)

    case stamped @ Stamped(_, _, KeyedEvent(orderId: OrderId, event: OrderEvent)) ⇒
      event match {
        case OrderDetached ⇒
          (eventsForMaster ? stamped) await 2 * askTimeout.duration.toJavaDuration  // blocking !!!
        case _ ⇒
          eventsForMaster ! stamped
      }
      handleEvent(orderId, event)
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
            idToOrder(orderId) = idToOrder(orderId).update(event)
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
          case Order.Join(joinOrderIds) ⇒
            for (joinOrderId ← joinOrderIds) {
              idToOrder -= joinOrderId
            }

          case state ⇒
            sys.error(s"Event $event recovered, but $orderId is in state $state")
        }

      case _ ⇒
    }

  def workflows = workflowRegister.workflows
  def orders = idToOrder.values
}
