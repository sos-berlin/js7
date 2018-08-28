package com.sos.jobscheduler.master

import cats.syntax.option._
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.JournalRecoverer
import com.sos.jobscheduler.core.filebased.Repo
import com.sos.jobscheduler.data.agent.{AgentId, AgentPath}
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.{FileBasedId, RepoEvent}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderFinished, OrderForked, OrderJoined, OrderStdWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.master.agent.{AgentEventId, AgentEventIdEvent}
import com.sos.jobscheduler.master.data.events.{MasterAgentEvent, MasterEvent}
import com.sos.jobscheduler.master.scheduledorder.{OrderScheduleEndedAt, OrderScheduleEvent}
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
private final class MasterJournalRecoverer(protected val journalMeta: JournalMeta[Event])
extends JournalRecoverer[Event]
{
  private var repo = Repo.empty
  private val idToOrder = mutable.Map[OrderId, Order[Order.State]]()
  private val agentToEventId = mutable.Map[AgentId, EventId]()
  private var orderScheduleEndedAt = none[Timestamp]

  protected def recoverSnapshot = {
    case order: Order[Order.State] ⇒
      idToOrder.insert(order.id → order)

    case AgentEventId(agentPath, eventId) ⇒
      agentToEventId(agentPath) = eventId

    case event: RepoEvent ⇒
      repo = repo.applyEvent(event).orThrow

    case OrderScheduleEndedAt(timestamp) ⇒
      orderScheduleEndedAt = Some(timestamp)
  }

  protected def recoverEvent = {
    case Stamped(_, _, keyedEvent) ⇒
      keyedEvent match {
        case KeyedEvent(_: NoKey, _: MasterEvent.MasterReady) ⇒
        case KeyedEvent(_: NoKey, OrderScheduleEvent.GeneratedUntil(instant)) ⇒
          orderScheduleEndedAt = Some(instant)

        case KeyedEvent(_: NoKey, event: RepoEvent) ⇒
          repo = repo.applyEvent(event).orThrow

        case KeyedEvent(orderId: OrderId, event: OrderEvent) ⇒
          event match {
            case event: OrderAdded ⇒
              idToOrder.insert(orderId → Order.fromOrderAdded(orderId, event))

            case OrderFinished ⇒
              idToOrder -= orderId

            case event: OrderCoreEvent ⇒
              handleForkJoinEvent(orderId, event)
              idToOrder(orderId) = idToOrder(orderId).update(event)

            case _: OrderStdWritten ⇒
          }

        case KeyedEvent(FileBasedId(a: AgentPath, v), AgentEventIdEvent(agentEventId)) ⇒
          agentToEventId(a % v) = agentEventId

        case KeyedEvent(_, _: MasterAgentEvent) ⇒
      }
  }

  private def handleForkJoinEvent(orderId: OrderId, event: OrderCoreEvent): Unit =  // TODO Duplicate with Agent's OrderJournalRecoverer
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

  def masterState: Option[MasterState] =
    hasJournal ? MasterState(lastRecoveredEventId, repo, idToOrder.values.toVector, agentToEventId.toMap, orderScheduleEndedAt)
}
