package js7.data.controller

import js7.base.crypt.Signed
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.utils.Collections.implicits._
import js7.data.agent.{AgentPath, AgentRef, AgentRefState, AgentRefStateEvent}
import js7.data.cluster.{ClusterEvent, ClusterStateSnapshot}
import js7.data.controller.ControllerEvent.{ControllerShutDown, ControllerTestEvent}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{JournalEvent, JournalState, JournaledState, JournaledStateBuilder, KeyedEvent, Stamped}
import js7.data.execution.workflow.WorkflowAndOrderRecovering.followUpRecoveredWorkflowsAndOrders
import js7.data.item.BasicItemEvent.{ItemAttachedStateChanged, ItemDeletionMarked, ItemDestroyed}
import js7.data.item.ItemAttachedState.{Detached, NotDetached}
import js7.data.item.SignedItemEvent.{SignedItemAdded, SignedItemChanged}
import js7.data.item.UnsignedSimpleItemEvent.{SimpleItemAdded, SimpleItemChanged}
import js7.data.item.{BasicItemEvent, InventoryItemEvent, InventoryItemKey, ItemAttachedState, Repo, SignableSimpleItem, SignableSimpleItemPath, SignedItemEvent, UnsignedSimpleItemEvent, VersionedEvent, VersionedItemId_}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderForked, OrderJoined, OrderLockEvent, OrderOffered, OrderRemoved, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{AllOrderWatchesState, OrderWatch, OrderWatchEvent, OrderWatchPath, OrderWatchState}
import js7.data.workflow.Workflow
import scala.collection.mutable

final class ControllerStateBuilder
extends JournaledStateBuilder[ControllerState]
{
  protected val S = ControllerState

  private var standards: JournaledState.Standards = JournaledState.Standards.empty
  private var controllerMetaState = ControllerMetaState.Undefined
  private var repo = Repo.empty
  private val idToOrder = mutable.Map.empty[OrderId, Order[Order.State]]
  private val pathToAgentRefState = mutable.Map.empty[AgentPath, AgentRefState]
  private val pathToLockState = mutable.Map.empty[LockPath, LockState]
  private var allOrderWatchesState = AllOrderWatchesState.empty
  private val idToSignedSimpleItem = mutable.Map.empty[SignableSimpleItemPath, Signed[SignableSimpleItem]]
  private val itemToAgentToAttachedState = mutable.Map.empty[InventoryItemKey, Map[AgentPath, ItemAttachedState.NotDetached]]

  protected def onInitializeState(state: ControllerState): Unit = {
    standards = state.standards
    controllerMetaState = state.controllerMetaState
    repo = state.repo
    idToOrder.clear()
    idToOrder ++= state.idToOrder
    pathToAgentRefState.clear()
    pathToAgentRefState ++= state.pathToAgentRefState
    pathToLockState ++= state.pathToLockState
    allOrderWatchesState = state.allOrderWatchesState
    idToSignedSimpleItem ++= state.idToSignedSimpleItem
    itemToAgentToAttachedState ++= state.itemToAgentToAttachedState
  }

  protected def onAddSnapshotObject = {
    case order: Order[Order.State] =>
      idToOrder.insert(order.id -> order)

    case event: VersionedEvent =>
      repo = repo.applyEvent(event).orThrow

    case event: BasicItemEvent =>
      repo = repo.applyBasicItemEvent(event).orThrow

    case agentRefState: AgentRefState =>
      pathToAgentRefState.insert(agentRefState.agentPath -> agentRefState)

    case lockState: LockState =>
      pathToLockState.insert(lockState.lock.path -> lockState)

    case signedItemAdded: SignedItemAdded =>
      onSignedItemAdded(signedItemAdded)

    case snapshot: OrderWatchState.Snapshot =>
      allOrderWatchesState = allOrderWatchesState.applySnapshot(snapshot).orThrow

    case ControllerState.ItemAttachedStateSnapshot(itemKey, agentToAttachedState) =>
      itemToAgentToAttachedState.insert(itemKey -> agentToAttachedState)

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
        case event: UnsignedSimpleItemEvent =>
          event match {
            case SimpleItemAdded(item) =>
              item match {
                case lock: Lock =>
                  pathToLockState.insert(lock.path -> LockState(lock))

                case agentRef: AgentRef =>
                  pathToAgentRefState.insert(agentRef.path -> AgentRefState(agentRef))

                case orderWatch: OrderWatch =>
                  allOrderWatchesState = allOrderWatchesState.addOrderWatch(orderWatch).orThrow
              }

            case SimpleItemChanged(item) =>
              item match {
                case lock: Lock =>
                  pathToLockState(lock.path) = pathToLockState(lock.path).copy(
                    lock = lock)

                case agentRef: AgentRef =>
                  pathToAgentRefState(agentRef.path) = pathToAgentRefState(agentRef.path).copy(
                    agentRef = agentRef)

                case orderWatch: OrderWatch =>
                  allOrderWatchesState = allOrderWatchesState.changeOrderWatch(orderWatch).orThrow
              }
          }

        case event: SignedItemEvent =>
          event match {
            case event: SignedItemAdded =>
              onSignedItemAdded(event)

            case SignedItemChanged(Signed(item, signedString)) =>
              item match {
                case jobResource: JobResource =>
                  idToSignedSimpleItem += jobResource.path -> Signed(jobResource, signedString)
              }
          }

        case event: BasicItemEvent.ForController =>
          event match {
            case event @ ItemAttachedStateChanged(id, agentPath, attachedState) =>
              id match {
                case id: OrderWatchPath =>
                  allOrderWatchesState = allOrderWatchesState
                    .updateAttachedState(id, agentPath, attachedState)
                    .orThrow

                case _: VersionedItemId_ =>
                  repo = repo.applyBasicItemEvent(event).orThrow

                case path: JobResourcePath =>
                  // TODO Code is similar to ControllerState
                  attachedState match {
                    case attachedState: NotDetached =>
                      itemToAgentToAttachedState += path ->
                        (itemToAgentToAttachedState.getOrElse(path, Map.empty) +
                          (agentPath -> attachedState))

                    case Detached =>
                      val updated = itemToAgentToAttachedState.getOrElse(path, Map.empty) - agentPath
                      if (updated.isEmpty)
                        itemToAgentToAttachedState -= path
                      else
                        itemToAgentToAttachedState += path -> updated
                  }

                case _ =>
                  throw Problem(s"Unexpected event: $keyedEvent").throwable
              }

            case ItemDeletionMarked(itemId) =>
              itemId match {
                case path: OrderWatchPath =>
                  allOrderWatchesState = allOrderWatchesState.markAsDeleted(path).orThrow

                case _ =>
                  throw Problem(s"Unexpected event: $keyedEvent").throwable
              }

            case ItemDestroyed(itemId) =>
              itemId match {
                case path: LockPath =>
                  pathToLockState -= path

                case path: AgentPath =>
                  pathToAgentRefState -= path

                case path: OrderWatchPath =>
                  allOrderWatchesState = allOrderWatchesState.removeOrderWatch(path)
              }
          }
      }

    case Stamped(_, _, KeyedEvent(name: AgentPath, event: AgentRefStateEvent)) =>
      pathToAgentRefState += name -> pathToAgentRefState(name).applyEvent(event).orThrow

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
          for (lockPath <- event.lockPaths) {
            pathToLockState(lockPath) = pathToLockState(lockPath).applyEvent(orderId <-: event).orThrow
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

    case Stamped(_, _, KeyedEvent(orderWatchPath: OrderWatchPath, event: OrderWatchEvent)) =>
      allOrderWatchesState = allOrderWatchesState.onOrderWatchEvent(orderWatchPath <-: event).orThrow

    case Stamped(_, _, KeyedEvent(_, _: ControllerShutDown)) =>
    case Stamped(_, _, KeyedEvent(_, ControllerTestEvent)) =>

    case Stamped(_, _, KeyedEvent(_: NoKey, event: JournalEvent)) =>
      standards = standards.copy(
        journalState = standards.journalState.applyEvent(event))

    case Stamped(_, _, KeyedEvent(_: NoKey, event: ClusterEvent)) =>
      standards = standards.copy(
        clusterState = standards.clusterState.applyEvent(event).orThrow)
  }

  private def onSignedItemAdded(added: SignedItemEvent.SignedItemAdded): Unit =
    added.signed.value match {
      case jobResource: JobResource =>
        idToSignedSimpleItem.insert(jobResource.path -> Signed(jobResource, added.signedString))
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
      pathToAgentRefState.toMap,
      pathToLockState.toMap,
      allOrderWatchesState,
      repo,
      idToSignedSimpleItem.toMap,
      itemToAgentToAttachedState.toMap,
      idToOrder.toMap)

  def journalState = standards.journalState

  def clusterState = standards.clusterState
}
