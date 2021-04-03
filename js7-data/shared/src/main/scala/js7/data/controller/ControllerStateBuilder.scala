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
import js7.data.item.CommonItemEvent.{ItemAttachedStateChanged, ItemDeletionMarked, ItemDestroyed}
import js7.data.item.SimpleItemEvent.{SimpleItemAdded, SimpleItemChanged}
import js7.data.item.{InventoryItemEvent, Repo, VersionedEvent}
import js7.data.lock.{Lock, LockId, LockState}
import js7.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderForked, OrderJoined, OrderLockEvent, OrderOffered, OrderRemoved, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{AllOrderWatchesState, OrderWatch, OrderWatchEvent, OrderWatchId, OrderWatchState}
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
  private var allOrderWatchesState = AllOrderWatchesState.empty

  protected def onInitializeState(state: ControllerState): Unit = {
    controllerMetaState = state.controllerMetaState
    standards = state.standards
    repo = state.repo
    idToOrder.clear()
    idToOrder ++= state.idToOrder
    idToAgentRefState.clear()
    idToAgentRefState ++= state.idToAgentRefState
    idToLockState ++= state.idToLockState
    allOrderWatchesState = state.allOrderWatchesState
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

    case snapshot: OrderWatchState.Snapshot =>
      allOrderWatchesState = allOrderWatchesState.applySnapshot(snapshot).orThrow

    case o: ControllerMetaState =>
      controllerMetaState = o

    case o: JournalState =>
      standards = standards.copy(journalState = o)

    case ClusterStateSnapshot(o) =>
      standards = standards.copy(clusterState = o)
  }

  override protected def onOnAllSnapshotsAdded() = {
    val (added, removed) = followUpRecoveredWorkflowsAndOrders(repo.idTo[Workflow], idToOrder.toMap)
    idToOrder ++= added
    idToOrder --= removed
    allOrderWatchesState = allOrderWatchesState.onEndOfRecovery.orThrow
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

    case Stamped(_, _, keyedEvent @ KeyedEvent(_: NoKey, event: InventoryItemEvent)) =>
      event match {
        case SimpleItemAdded(item) =>
          item match {
            case lock: Lock =>
              idToLockState.insert(lock.id -> LockState(lock))

            case agentRef: AgentRef =>
              idToAgentRefState.insert(agentRef.id -> AgentRefState(agentRef))

            case orderWatch: OrderWatch =>
              allOrderWatchesState = allOrderWatchesState.addOrderWatch(orderWatch).orThrow
          }

        case SimpleItemChanged(item) =>
          item match {
            case lock: Lock =>
              idToLockState(lock.id) = idToLockState(lock.id).copy(
                lock = lock)

            case agentRef: AgentRef =>
              idToAgentRefState(agentRef.id) = idToAgentRefState(agentRef.id).copy(
                agentRef = agentRef)

            case orderWatch: OrderWatch =>
              allOrderWatchesState = allOrderWatchesState.changeOrderWatch(orderWatch).orThrow
          }

        case ItemAttachedStateChanged(id, agentId, attachedState) =>
          id match {
            case id: OrderWatchId =>
              allOrderWatchesState = allOrderWatchesState
                .updateAttachedState(id, agentId, attachedState)
                .orThrow

            case _ =>
              throw Problem(s"Unexpected event: $keyedEvent").throwable
          }

        case ItemDeletionMarked(itemId) =>
          itemId match {
            case id: OrderWatchId =>
              allOrderWatchesState = allOrderWatchesState.markAsDeleted(id).orThrow

            case _ =>
              throw Problem(s"Unexpected event: $keyedEvent").throwable
          }

        case ItemDestroyed(itemId) =>
          itemId match {
            case id: LockId =>
              idToLockState -= id

            case id: AgentId =>
              idToAgentRefState -= id

            case id: OrderWatchId =>
              allOrderWatchesState = allOrderWatchesState.removeOrderWatch(id)
          }
      }

    case Stamped(_, _, KeyedEvent(name: AgentId, event: AgentRefStateEvent)) =>
      idToAgentRefState += name -> idToAgentRefState(name).applyEvent(event).orThrow

    case Stamped(_, _, KeyedEvent(orderId: OrderId, event: OrderEvent)) =>
      event match {
        case orderAdded: OrderAdded =>
          idToOrder.insert(orderId -> Order.fromOrderAdded(orderId, orderAdded))
          allOrderWatchesState = allOrderWatchesState.onOrderAdded(orderId <-: orderAdded).orThrow

        case orderRemoved: OrderRemoved =>
          for (order <- idToOrder.remove(orderId)) {
            for (externalOrderKey <- order.externalOrderKey)
              allOrderWatchesState = allOrderWatchesState
                .onOrderEvent(externalOrderKey, orderId <-: orderRemoved)
                .orThrow
          }

        case event: OrderLockEvent =>
          for (lockId <- event.lockIds) {
            idToLockState(lockId) = idToLockState(lockId).applyEvent(orderId <-: event).orThrow
          }
          idToOrder(orderId) = idToOrder(orderId).applyEvent(event).orThrow

        case event: OrderCoreEvent =>
          handleForkJoinEvent(orderId, event)
          val order = idToOrder(orderId)
          idToOrder(orderId) = order.applyEvent(event).orThrow

          for (externalOrderKey <- order.externalOrderKey) {
            allOrderWatchesState = allOrderWatchesState
              .onOrderEvent(externalOrderKey, orderId <-: event)
              .orThrow
          }

        case _: OrderStdWritten =>
      }

    case Stamped(_, _, KeyedEvent(orderWatchId: OrderWatchId, event: OrderWatchEvent)) =>
      allOrderWatchesState = allOrderWatchesState.onOrderWatchEvent(orderWatchId <-: event).orThrow

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

  def result() =
    ControllerState(
      eventId = eventId,
      standards,
      controllerMetaState,
      idToAgentRefState.toMap,
      idToLockState.toMap,
      allOrderWatchesState,
      repo,
      idToOrder.toMap)

  def journalState = standards.journalState

  def clusterState = standards.clusterState
}
