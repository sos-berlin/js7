package com.sos.jobscheduler.master

import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.core.event.state.JournaledStateBuilder
import com.sos.jobscheduler.core.filebased.Repo
import com.sos.jobscheduler.core.workflow.Recovering.followUpRecoveredSnapshots
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterState}
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{EventId, JournalEvent, JournalState, JournaledState, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.master.MasterFileBaseds
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderCancelled, OrderCoreEvent, OrderFinished, OrderForked, OrderJoined, OrderOffered, OrderStdWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.data.MasterSnapshots.MasterMetaState
import com.sos.jobscheduler.master.data.agent.{AgentEventIdEvent, AgentSnapshot}
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.{AgentCouplingFailed, AgentReady, AgentRegisteredMaster}
import com.sos.jobscheduler.master.data.events.MasterEvent.{MasterShutDown, MasterTestEvent}
import com.sos.jobscheduler.master.data.events.{MasterAgentEvent, MasterEvent}
import scala.collection.mutable

final class MasterStateBuilder
extends JournaledStateBuilder[MasterState]
{
  private var standards: JournaledState.Standards = JournaledState.Standards.empty
  private var masterMetaState = MasterMetaState.Undefined
  private var repo = Repo(MasterFileBaseds.jsonCodec)
  private val idToOrder = mutable.Map[OrderId, Order[Order.State]]()
  private val pathToAgent = mutable.Map[AgentRefPath, AgentSnapshot]()

  protected def onInitializeState(state: MasterState): Unit = {
    masterMetaState = state.masterMetaState
    standards = state.standards
    repo = state.repo
    idToOrder.clear()
    idToOrder ++= state.idToOrder
    pathToAgent.clear()
    pathToAgent ++= state.pathToAgentSnapshot
  }

  protected def onAddSnapshot = {
    case order: Order[Order.State] =>
      idToOrder.insert(order.id -> order)

    case event: RepoEvent =>
      repo = repo.applyEvent(event).orThrow

    case snapshot: AgentSnapshot =>
      pathToAgent.insert(snapshot.agentRefPath -> snapshot)

    case o: MasterMetaState =>
      masterMetaState = o

    case o: JournalState =>
      standards = standards.copy(journalState = o)

    case ClusterState.ClusterStateSnapshot(o) =>
      standards = standards.copy(clusterState = o)
  }

  def onOnAllSnapshotsAdded() = {
    val (added, removed) = followUpRecoveredSnapshots(repo.idTo[Workflow], idToOrder.toMap)
    idToOrder ++= added.map(o => o.id -> o)
    idToOrder --= removed
  }

  protected def onAddEvent = {
    case Stamped(_, _, KeyedEvent(_: NoKey, MasterEvent.MasterInitialized(masterId, startedAt))) =>
      masterMetaState = masterMetaState.copy(
        masterId = masterId,
        startedAt = startedAt)

    case Stamped(_, _, KeyedEvent(_: NoKey, _: MasterEvent.MasterReady)) =>

    case Stamped(_, _, KeyedEvent(_: NoKey, event: RepoEvent)) =>
      repo = repo.applyEvent(event).orThrow

    case Stamped(_, _, KeyedEvent(agentRefPath: AgentRefPath, event: MasterAgentEvent)) =>
      event match {
        case AgentRegisteredMaster(agentRunId) =>
          pathToAgent.insert(agentRefPath -> AgentSnapshot(agentRefPath, Some(agentRunId), eventId = EventId.BeforeFirst))

        case _: AgentReady | _: AgentCouplingFailed =>
      }

    case Stamped(_, _, KeyedEvent(a: AgentRefPath, AgentEventIdEvent(agentEventId))) =>
      // Preceding AgentSnapshot is required (see recoverSnapshot)
      pathToAgent(a) = pathToAgent(a).copy(eventId = agentEventId)

    case Stamped(_, _, KeyedEvent(orderId: OrderId, event: OrderEvent)) =>
      event match {
        case event: OrderAdded =>
          idToOrder.insert(orderId -> Order.fromOrderAdded(orderId, event))

        case OrderFinished | OrderCancelled =>
          idToOrder -= orderId

        case event: OrderCoreEvent =>
          handleForkJoinEvent(orderId, event)
          idToOrder(orderId) = idToOrder(orderId).update(event).orThrow

        case _: OrderStdWritten =>
      }

    case Stamped(_, _, KeyedEvent(_, _: MasterShutDown)) =>
    case Stamped(_, _, KeyedEvent(_, MasterTestEvent)) =>

    case Stamped(_, _, KeyedEvent(_: NoKey, event: JournalEvent)) =>
      standards = standards.copy(
        journalState = standards.journalState.applyEvent(event))

    case Stamped(_, _, KeyedEvent(_: NoKey, event: ClusterEvent)) =>
      standards = standards.copy(
        clusterState = standards.clusterState.applyEvent(event).orThrow)
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

          case awaiting: Order.Awaiting =>
            // Offered order is being kept ???
            //idToOrder -= awaiting.offeredOrderId

          case state =>
            sys.error(s"Event $event recovered, but $orderId is in state $state")
        }

      case event: OrderOffered =>
        val offered = idToOrder(orderId).newOfferedOrder(event)
        idToOrder.insert(offered.id -> offered)
        idToOrder(orderId) = idToOrder(orderId).update(event).orThrow

      case _ =>
    }

  def state =
    MasterState(
      eventId = eventId,
      standards,
      masterMetaState,
      repo,
      pathToAgent.toMap,
      idToOrder.toMap)

  def journalState = standards.journalState

  def clusterState = standards.clusterState
}

