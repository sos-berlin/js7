package com.sos.jobscheduler.master.order

import akka.actor.ActorRef
import com.sos.jobscheduler.base.utils.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderForked, OrderJoined, OrderStdWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.master.{AgentEventId, AgentEventIdEvent}
import com.sos.jobscheduler.shared.event.journal.{JournalRecoverer, KeyedJournalingActor}
import java.nio.file.Path
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
private[order] class MasterJournalRecoverer(protected val journalFile: Path, orderScheduleGenerator: ActorRef)(implicit protected val sender: ActorRef)
extends JournalRecoverer[Event] {
  protected val journalMeta = MasterOrderKeeper.journalMeta(compressWithGzip = false/*irrelevant, we read*/)
  private val idToOrder = mutable.Map[OrderId, Order[Order.State]]()
  private val _agentToEventId = mutable.Map[AgentPath, EventId]()

  def recoverSnapshot = {
    case o: OrderScheduleEndedAt ⇒
      orderScheduleGenerator ! KeyedJournalingActor.Input.RecoverFromSnapshot(o)

    case order: Order[Order.State] ⇒
      idToOrder.insert(order.id → order)

    case AgentEventId(agentPath, eventId) ⇒
      _agentToEventId(agentPath) = eventId
  }

  def recoverEvent = {
    case stamped @ Stamped(_, _, KeyedEvent(_: NoKey.type, _: OrderScheduleEvent)) ⇒
      orderScheduleGenerator ! KeyedJournalingActor.Input.RecoverFromEvent(stamped)

    case Stamped(_, _, KeyedEvent(orderId: OrderId, event: OrderEvent)) ⇒
      event match {
        case event: OrderAdded ⇒
          idToOrder.insert(orderId → Order.fromOrderAdded(orderId, event))

        case event: OrderCoreEvent ⇒
          handleForkJoinEvent(orderId, event)
          idToOrder(orderId) = idToOrder(orderId).update(event)

        case OrderStdWritten(t, chunk) ⇒
          // TODO What to do with Order output?
          //logger.debug(s"$orderId recovered $t: ${chunk.trim}")
      }

    case Stamped(_, _, KeyedEvent(agentPath: AgentPath, AgentEventIdEvent(agentEventId))) ⇒
      _agentToEventId(agentPath) = agentEventId
  }

  private def handleForkJoinEvent(orderId: OrderId, event: OrderCoreEvent): Unit =  // TODO Duplicate with Agent's OrderJournalRecoverer
    event match {
      case event: OrderForked ⇒
        for (childOrder ← idToOrder(orderId).newForkedOrders(event)) {
          idToOrder.insert(childOrder.id → childOrder)
        }
        idToOrder(orderId) = idToOrder(orderId).update(event)

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

  def orders = idToOrder.values.toVector

  def agentToEventId = _agentToEventId.toMap
}

object MasterJournalRecoverer {
  private val logger = Logger(getClass)
}
