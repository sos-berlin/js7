package com.sos.jobscheduler.master

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.JournalRecoverer
import com.sos.jobscheduler.core.filebased.Repo
import com.sos.jobscheduler.core.workflow.Recovering.followUpRecoveredSnapshots
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.master.MasterFileBaseds
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderCanceled, OrderCoreEvent, OrderFinished, OrderForked, OrderJoined, OrderStdWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.data.agent.{AgentEventId, AgentEventIdEvent}
import com.sos.jobscheduler.master.data.events.MasterEvent.MasterTestEvent
import com.sos.jobscheduler.master.data.events.{MasterAgentEvent, MasterEvent}
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
private final class MasterJournalRecoverer(protected val journalMeta: JournalMeta[Event])
extends JournalRecoverer[Event]
{
  private var repo = Repo(MasterFileBaseds.jsonCodec)
  private val idToOrder = mutable.Map[OrderId, Order[Order.State]]()
  private val agentToEventId = mutable.Map[AgentRefPath, EventId]()

  protected def recoverSnapshot = {
    case order: Order[Order.State] =>
      idToOrder.insert(order.id -> order)

    case AgentEventId(agentRefPath, eventId) =>
      agentToEventId(agentRefPath) = eventId

    case event: RepoEvent =>
      repo = repo.applyEvent(event).orThrow
  }

  override protected def onAllSnapshotRecovered() = {
    val (added, removed) = followUpRecoveredSnapshots(repo.idTo[Workflow], idToOrder.toMap)
    idToOrder ++= added.map(o => o.id -> o)
    idToOrder --= removed
  }

  protected def recoverEvent = {
    case Stamped(_, _, keyedEvent) =>
      keyedEvent match {
        case KeyedEvent(_: NoKey, _: MasterEvent.MasterReady) =>

        case KeyedEvent(_: NoKey, event: RepoEvent) =>
          repo = repo.applyEvent(event).orThrow

        case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
          event match {
            case event: OrderAdded =>
              idToOrder.insert(orderId -> Order.fromOrderAdded(orderId, event))

            case OrderFinished | OrderCanceled =>
              idToOrder -= orderId

            case event: OrderCoreEvent =>
              handleForkJoinEvent(orderId, event)
              idToOrder(orderId) = idToOrder(orderId).update(event).orThrow  // 🔥 ProblemException

            case _: OrderStdWritten =>
          }

        case KeyedEvent(a: AgentRefPath, AgentEventIdEvent(agentEventId)) =>
          agentToEventId(a) = agentEventId

        case KeyedEvent(_, _: MasterAgentEvent) =>
        case KeyedEvent(_, MasterTestEvent) =>

        case _ => sys.error(s"Unknown event recovered from journal: $keyedEvent")
      }
  }

  private def handleForkJoinEvent(orderId: OrderId, event: OrderCoreEvent): Unit =  // TODO Duplicate with Agent's OrderJournalRecoverer
    event match {
      case event: OrderForked =>
        for (childOrder <- idToOrder(orderId).newForkedOrders(event)) {
          idToOrder.insert(childOrder.id -> childOrder)
        }

      case event: OrderJoined =>
        idToOrder(orderId).state match {
          case forked: Order.Forked =>
            idToOrder --= forked.childOrderIds

          case state =>
            sys.error(s"Event $event recovered, but $orderId is in state $state")
        }

      case _ =>
    }

  def masterState: Option[MasterState] =
    hasJournal ? MasterState(lastRecoveredEventId, repo, idToOrder.values.toImmutableSeq, agentToEventId.toMap)
}
