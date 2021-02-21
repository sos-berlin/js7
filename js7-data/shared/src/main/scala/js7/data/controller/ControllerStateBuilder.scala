package js7.data.controller

import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.utils.Collections.implicits._
import js7.data.agent.{AgentId, AgentRef, AgentRefState, AgentRefStateEvent}
import js7.data.cluster.{ClusterEvent, ClusterStateSnapshot}
import js7.data.controller.ControllerEvent.{ControllerShutDown, ControllerTestEvent}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{JournalEvent, JournalState, JournaledState, JournaledStateBuilder, KeyedEvent, Stamped}
import js7.data.execution.workflow.WorkflowAndOrderRecovering.followUpRecoveredWorkflowsAndOrders
import js7.data.item.SimpleItemEvent.{SimpleItemAdded, SimpleItemChanged, SimpleItemDeleted}
import js7.data.item.{Repo, SimpleItemEvent, VersionedEvent}
import js7.data.lock.{Lock, LockId, LockState}
import js7.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderForked, OrderJoined, OrderLockEvent, OrderOffered, OrderRemoved, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.workflow.Workflow
import scala.collection.mutable

final class ControllerStateBuilder
extends JournaledStateBuilder[ControllerState]
{
  private var standards: JournaledState.Standards = JournaledState.Standards.empty
  private var controllerMetaState = ControllerMetaState.Undefined
  private var repo = Repo.empty
  private val idToOrder = mutable.Map.empty[OrderId, Order[Order.State]]
  private val idToAgentRefState = mutable.Map.empty[AgentId, AgentRefState]
  private val idToLockState = mutable.Map.empty[LockId, LockState]

  protected def onInitializeState(state: ControllerState): Unit = {
    controllerMetaState = state.controllerMetaState
    standards = state.standards
    repo = state.repo
    idToOrder.clear()
    idToOrder ++= state.idToOrder
    idToAgentRefState.clear()
    idToAgentRefState ++= state.idToAgentRefState
    idToLockState ++= state.idToLockState
  }

  protected def onAddSnapshotObject = {
    case order: Order[Order.State] =>
      idToOrder.insert(order.id -> order)

    case event: VersionedEvent =>
      repo = repo.applyEvent(event).orThrow

    case agentRefState: AgentRefState =>
      idToAgentRefState.insert(agentRefState.agentId -> agentRefState)

    case lockState: LockState =>
      idToLockState.insert(lockState.lock.id -> lockState)

    case o: ControllerMetaState =>
      controllerMetaState = o

    case o: JournalState =>
      standards = standards.copy(journalState = o)

    case ClusterStateSnapshot(o) =>
      standards = standards.copy(clusterState = o)
  }

  protected def onOnAllSnapshotsAdded() = {
    val (added, removed) = followUpRecoveredWorkflowsAndOrders(repo.idTo[Workflow], idToOrder.toMap)
    idToOrder ++= added.map(o => o.id -> o)
    idToOrder --= removed
  }

  protected def onAddEvent = {
    case Stamped(_, _, KeyedEvent(_: NoKey, ControllerEvent.ControllerInitialized(controllerId, startedAt))) =>
      controllerMetaState = controllerMetaState.copy(
        controllerId = controllerId,
        startedAt = startedAt)

    case Stamped(_, _, KeyedEvent(_: NoKey, event: ControllerEvent.ControllerReady)) =>
      controllerMetaState = controllerMetaState.copy(
        timezone = event.timezone)

    case Stamped(_, _, KeyedEvent(_: NoKey, event: VersionedEvent)) =>
      repo = repo.applyEvent(event).orThrow

    case Stamped(_, _, KeyedEvent(_: NoKey, event: SimpleItemEvent)) =>
      event match {
        case SimpleItemAdded(lock: Lock) =>
          idToLockState.insert(lock.id -> LockState(lock))

        case SimpleItemChanged(lock: Lock) =>
          idToLockState(lock.id) = idToLockState(lock.id).copy(
            lock = lock)

        case SimpleItemDeleted(_: LockId) =>
          throw Problem("Locks are not deletable (in this version)").throwable  // TODO

        case SimpleItemAdded(agentRef: AgentRef) =>
          idToAgentRefState.insert(agentRef.id -> AgentRefState(agentRef))

        case SimpleItemChanged(agentRef: AgentRef) =>
          idToAgentRefState(agentRef.id) = idToAgentRefState(agentRef.id).copy(
            agentRef = agentRef)

        case SimpleItemDeleted(_: AgentId) =>
          throw Problem("AgentRefs are not deletable (in this version)").throwable  // TODO
      }

    case Stamped(_, _, KeyedEvent(name: AgentId, event: AgentRefStateEvent)) =>
      idToAgentRefState += name -> idToAgentRefState(name).applyEvent(event).orThrow

    case Stamped(_, _, KeyedEvent(orderId: OrderId, event: OrderEvent)) =>
      event match {
        case event: OrderAdded =>
          idToOrder += orderId -> Order.fromOrderAdded(orderId, event)

        case OrderRemoved =>
          idToOrder -= orderId

        case event: OrderLockEvent =>
          for (lockId <- event.lockIds) {
            idToLockState(lockId) = idToLockState(lockId).applyEvent(orderId <-: event).orThrow
          }
          idToOrder(orderId) = idToOrder(orderId).applyEvent(event).orThrow

        case event: OrderCoreEvent =>
          handleForkJoinEvent(orderId, event)
          idToOrder(orderId) = idToOrder(orderId).applyEvent(event).orThrow

        case _: OrderStdWritten =>
      }

    case Stamped(_, _, KeyedEvent(_, _: ControllerShutDown)) =>
    case Stamped(_, _, KeyedEvent(_, ControllerTestEvent)) =>

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
          idToOrder += childOrder.id -> childOrder
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
        idToOrder += offered.id -> offered
        idToOrder(orderId) = idToOrder(orderId).applyEvent(event).orThrow

      case _ =>
    }

  def state =
    ControllerState(
      eventId = eventId,
      standards,
      controllerMetaState,
      idToAgentRefState.toMap,
      idToLockState.toMap,
      repo,
      idToOrder.toMap)

  def journalState = standards.journalState

  def clusterState = standards.clusterState
}
