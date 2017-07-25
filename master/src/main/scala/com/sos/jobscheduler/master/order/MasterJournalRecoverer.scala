package com.sos.jobscheduler.master.order

import akka.actor.ActorRef
import com.sos.jobscheduler.common.scalautil.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderStdWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.master.order.MasterJournalRecoverer._
import com.sos.jobscheduler.master.{AgentEventId, AgentEventIdEvent}
import com.sos.jobscheduler.shared.event.journal.{JsonJournalRecoverer, KeyedJournalingActor}
import java.nio.file.Path
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
private[order] class MasterJournalRecoverer(protected val journalFile: Path, orderScheduleGenerator: ActorRef)(implicit protected val sender: ActorRef)
extends JsonJournalRecoverer[Event] {
  protected val jsonJournalMeta = MasterOrderKeeper.MyJournalMeta
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
    case stamped @ Stamped(_, KeyedEvent(_: NoKey.type, _: OrderScheduleEvent)) ⇒
      orderScheduleGenerator ! KeyedJournalingActor.Input.RecoverFromEvent(stamped)

    case Stamped(_, KeyedEvent(orderId: OrderId, event: OrderEvent)) ⇒
      event match {
        case event: OrderAdded ⇒
          idToOrder.insert(orderId → Order.fromOrderAdded(orderId, event))
        case event: OrderCoreEvent ⇒
          idToOrder(orderId) = idToOrder(orderId).update(event)
        case OrderStdWritten(t, chunk) ⇒
          // TODO What to do with Order output?
          logger.debug(s"$orderId recovered $t: ${chunk.trim}")
      }

    case Stamped(_, KeyedEvent(agentPath: AgentPath, AgentEventIdEvent(agentEventId))) ⇒
      _agentToEventId(agentPath) = agentEventId
  }

  def orders = idToOrder.values.toVector

  def agentToEventId = _agentToEventId.toMap
}

object MasterJournalRecoverer {
  private val logger = Logger(getClass)
}
