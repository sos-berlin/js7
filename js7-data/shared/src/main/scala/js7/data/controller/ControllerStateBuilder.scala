package js7.data.controller

import js7.base.crypt.Signed
import js7.base.problem.Checked._
import js7.base.utils.Collections.implicits._
import js7.data.agent.{AgentPath, AgentRef, AgentRefState, AgentRefStateEvent}
import js7.data.cluster.{ClusterEvent, ClusterStateSnapshot}
import js7.data.controller.ControllerEvent.{ControllerShutDown, ControllerTestEvent}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{JournalEvent, JournalState, JournaledState, JournaledStateBuilder, KeyedEvent, Stamped}
import js7.data.execution.workflow.WorkflowAndOrderRecovering.followUpRecoveredWorkflowsAndOrders
import js7.data.item.BasicItemEvent.{ItemAttachedStateChanged, ItemDeleted, ItemDeletionMarked}
import js7.data.item.ItemAttachedState.{Detached, NotDetached}
import js7.data.item.SignedItemEvent.{SignedItemAdded, SignedItemChanged}
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemChanged}
import js7.data.item.{BasicItemEvent, InventoryItemEvent, InventoryItemKey, ItemAttachedState, Repo, SignableSimpleItem, SignableSimpleItemPath, SignedItemEvent, UnsignedSimpleItemEvent, VersionedEvent, VersionedItemId_}
import js7.data.job.JobResource
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderDeleted, OrderForked, OrderJoined, OrderLockEvent, OrderOffered, OrderStdWritten}
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
  private val itemToAgentToAttachedState = mutable.Map.empty[InventoryItemKey, Map[AgentPath, ItemAttachedState.NotDetached]]
  private val deletionMarkedItems = mutable.Set[InventoryItemKey]()
  private val pathToSignedSimpleItem = mutable.Map.empty[SignableSimpleItemPath, Signed[SignableSimpleItem]]

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
    pathToSignedSimpleItem ++= state.pathToSignedSimpleItem
    itemToAgentToAttachedState ++= state.itemToAgentToAttachedState
  }

  protected def onAddSnapshotObject = {
    case order: Order[Order.State] =>
      idToOrder.insert(order.id -> order)

    case event: VersionedEvent =>
      repo = repo.applyEvent(event).orThrow

    case agentRefState: AgentRefState =>
      pathToAgentRefState.insert(agentRefState.agentPath -> agentRefState)

    case lockState: LockState =>
      pathToLockState.insert(lockState.lock.path -> lockState)

    case signedItemAdded: SignedItemAdded =>
      onSignedItemAdded(signedItemAdded)

    case UnsignedSimpleItemAdded(orderWatch: OrderWatch) =>
      allOrderWatchesState = allOrderWatchesState.addOrderWatch(orderWatch).orThrow

    case snapshot: OrderWatchState.ExternalOrderSnapshot =>
      allOrderWatchesState = allOrderWatchesState.applySnapshot(snapshot).orThrow

    case ItemAttachedStateChanged(key: InventoryItemKey, agentPath, attachedState: NotDetached) =>
      itemToAgentToAttachedState +=
        key -> (itemToAgentToAttachedState.getOrElse(key, Map.empty) + (agentPath -> attachedState))

    case ItemDeletionMarked(itemKey) =>
      deletionMarkedItems += itemKey

    case o: ControllerMetaState =>
      controllerMetaState = o

    case o: JournalState =>
      standards = standards.copy(journalState = o)

    case ClusterStateSnapshot(o) =>
      standards = standards.copy(clusterState = o)
  }

  override protected def onOnAllSnapshotsAdded() = {
    val (added, deleted) = followUpRecoveredWorkflowsAndOrders(repo.idTo[Workflow], idToOrder.toMap)
    idToOrder ++= added
    idToOrder --= deleted
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
            case UnsignedSimpleItemAdded(item) =>
              item match {
                case lock: Lock =>
                  pathToLockState.insert(lock.path -> LockState(lock))

                case agentRef: AgentRef =>
                  pathToAgentRefState.insert(agentRef.path -> AgentRefState(agentRef))

                case orderWatch: OrderWatch =>
                  allOrderWatchesState = allOrderWatchesState.addOrderWatch(orderWatch).orThrow
              }

            case UnsignedSimpleItemChanged(item) =>
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
                  pathToSignedSimpleItem += jobResource.path -> Signed(jobResource, signedString)
              }
          }

        case event: BasicItemEvent.ForController =>
          event match {
            case ItemAttachedStateChanged(itemKey, agentPath, attachedState) =>
              attachedState match {
                case attachedState: NotDetached =>
                  itemToAgentToAttachedState += itemKey ->
                    (itemToAgentToAttachedState.getOrElse(itemKey, Map.empty) +
                      (agentPath -> attachedState))

                case Detached =>
                  val updated = itemToAgentToAttachedState.getOrElse(itemKey, Map.empty) - agentPath
                  if (updated.isEmpty)
                    itemToAgentToAttachedState -= itemKey
                  else
                    itemToAgentToAttachedState += itemKey -> updated
              }

            case ItemDeletionMarked(itemKey) =>
              deletionMarkedItems += itemKey

            case ItemDeleted(itemKey) =>
              itemKey match {
                case id: VersionedItemId_ =>
                  repo = repo.deleteItem(id).orThrow

                case path: OrderWatchPath =>
                  deletionMarkedItems -= path
                  allOrderWatchesState = allOrderWatchesState.removeOrderWatch(path)

                case path: LockPath =>
                  pathToLockState -= path

                case agentPath: AgentPath =>
                  pathToAgentRefState -= agentPath
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

        case orderDeleted: OrderDeleted =>
          for (order <- idToOrder.remove(orderId)) {
            for (externalOrderKey <- order.externalOrderKey)
              allOrderWatchesState = allOrderWatchesState
                .onOrderEvent(externalOrderKey, orderId <-: orderDeleted)
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
        pathToSignedSimpleItem.insert(jobResource.path -> Signed(jobResource, added.signedString))
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
      pathToSignedSimpleItem.toMap,
      itemToAgentToAttachedState.toMap,
      deletionMarkedItems.toSet,
      idToOrder.toMap)

  def journalState = standards.journalState

  def clusterState = standards.clusterState
}
