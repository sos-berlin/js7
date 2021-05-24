package js7.data.controller

import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.Signed
import js7.base.problem.Problem
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.{AgentPath, AgentRef, AgentRefState, AgentRefStateEvent}
import js7.data.cluster.{ClusterEvent, ClusterStateSnapshot}
import js7.data.controller.ControllerEvent.{ControllerShutDown, ControllerTestEvent}
import js7.data.controller.ControllerState.ItemAttachedStateSnapshot
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{Event, EventId, JournalEvent, JournalHeader, JournalState, JournaledState, KeyedEvent, KeyedEventTypedJsonCodec, SnapshotMeta}
import js7.data.item.BasicItemEvent.{ItemAttachedStateChanged, ItemDeletionMarked, ItemDestroyed}
import js7.data.item.ItemAttachedState.{Attachable, Attached, Detachable, Detached, NotDetached}
import js7.data.item.SignedItemEvent.{SignedItemAdded, SignedItemChanged}
import js7.data.item.UnsignedSimpleItemEvent.{SimpleItemAdded, SimpleItemChanged}
import js7.data.item.{BasicItemEvent, InventoryItem, InventoryItemEvent, InventoryItemKey, ItemAttachedState, ItemRevision, Repo, SignableSimpleItem, SignableSimpleItemPath, SignedItemEvent, SimpleItem, SimpleItemPath, UnsignedSimpleItemEvent, UnsignedSimpleItemPath, VersionedEvent, VersionedItemId_}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderForked, OrderJoined, OrderLockEvent, OrderOffered, OrderRemoveMarked, OrderRemoved, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{AllOrderWatchesState, FileWatch, OrderWatch, OrderWatchEvent, OrderWatchPath, OrderWatchState}
import js7.data.workflow.Workflow
import monix.reactive.Observable
import scala.collection.MapView

/**
  * @author Joacim Zschimmer
  */
final case class ControllerState(
  eventId: EventId,
  standards: JournaledState.Standards,
  controllerMetaState: ControllerMetaState,
  pathToAgentRefState: Map[AgentPath, AgentRefState],
  pathToLockState: Map[LockPath, LockState],
  allOrderWatchesState: AllOrderWatchesState,
  repo: Repo,
  idToSignedSimpleItem: Map[SignableSimpleItemPath, Signed[SignableSimpleItem]],
  itemToAgentToAttachedState: Map[InventoryItemKey, Map[AgentPath, ItemAttachedState.NotDetached]],
  idToOrder: Map[OrderId, Order[Order.State]])
extends JournaledState[ControllerState]
{
  def companion = ControllerState

  def estimatedSnapshotSize: Int =
    1 +
    standards.snapshotSize +
    controllerMetaState.isDefined.toInt +
    repo.estimatedEventCount +
    pathToAgentRefState.size +
    pathToLockState.size +
    allOrderWatchesState.estimatedSnapshotSize +
    idToSignedSimpleItem.size +
    itemToAgentToAttachedState.values.view.map(_.size).sum +
    idToOrder.size

  def toSnapshotObservable: Observable[Any] =
    Observable.pure(SnapshotEventId(eventId)) ++
    standards.toSnapshotObservable ++
    Observable.fromIterable(controllerMetaState.isDefined ? controllerMetaState) ++
    Observable.fromIterable(pathToAgentRefState.values) ++
    Observable.fromIterable(pathToLockState.values) ++
    allOrderWatchesState.toSnapshot/*TODO Separate Item from its state?*/ ++
    Observable.fromIterable(idToSignedSimpleItem.values).map(SignedItemAdded(_)) ++
    Observable.fromIterable(repo.toEvents) ++
    Observable.fromIterable(itemToAgentToAttachedState).map(o => ItemAttachedStateSnapshot(o._1, o._2)) ++
    Observable.fromIterable(idToOrder.values)

  def withEventId(eventId: EventId) =
    copy(eventId = eventId)

  def withStandards(standards: JournaledState.Standards) =
    copy(standards = standards)

  def applyEvent(keyedEvent: KeyedEvent[Event]) = keyedEvent match {
    case KeyedEvent(_: NoKey, ControllerEvent.ControllerInitialized(controllerId, startedAt)) =>
      Right(copy(controllerMetaState = controllerMetaState.copy(
        controllerId = controllerId,
        startedAt = startedAt)))

    case KeyedEvent(_: NoKey, event: ControllerEvent.ControllerReady) =>
      Right(copy(controllerMetaState = controllerMetaState.copy(
        timezone = event.timezone)))

    case KeyedEvent(_: NoKey, _: ControllerEvent.ControllerShutDown) =>
      Right(this)

    case KeyedEvent(_: NoKey, ControllerEvent.ControllerTestEvent) =>
      Right(this)


    case KeyedEvent(_: NoKey, event: InventoryItemEvent) =>
      event match {
        case event: UnsignedSimpleItemEvent =>
          event match {
            case SimpleItemAdded(item) =>
              item match {
                case lock: Lock =>
                  for (o <- pathToLockState.insert(lock.path -> LockState(lock))) yield
                    copy(pathToLockState = o)

                case agentRef: AgentRef =>
                  for (o <- pathToAgentRefState.insert(agentRef.path -> AgentRefState(agentRef)))
                    yield copy(
                      pathToAgentRefState = o)

                case orderWatch: OrderWatch =>
                  for (o <- allOrderWatchesState.addOrderWatch(orderWatch)) yield
                    copy(allOrderWatchesState = o)
              }

            case SimpleItemChanged(item) =>
              item match {
                case lock: Lock =>
                  for (lockState <- pathToLockState.checked(lock.path))
                    yield copy(
                      pathToLockState = pathToLockState + (lock.path -> lockState.copy(
                        lock = lock)))

                case agentRef: AgentRef =>
                  for (agentRefState <- pathToAgentRefState.checked(agentRef.path))
                    yield copy(
                      pathToAgentRefState = pathToAgentRefState + (agentRef.path -> agentRefState.copy(
                        agentRef = agentRef)))

                case orderWatch: OrderWatch =>
                  allOrderWatchesState.changeOrderWatch(orderWatch)
                    .map(o => copy(
                      allOrderWatchesState = o))
              }
          }

        case event: SignedItemEvent =>
          event match {
            case SignedItemAdded(Signed(item, signedString)) =>
              item match {
                case jobResource: JobResource =>
                  for (o <- idToSignedSimpleItem.insert(jobResource.path -> Signed(jobResource, signedString))) yield
                    copy(idToSignedSimpleItem = o)
              }

            case SignedItemChanged(Signed(item, signedString)) =>
              item match {
                case jobResource: JobResource =>
                  Right(copy(
                    idToSignedSimpleItem = idToSignedSimpleItem + (jobResource.path -> Signed(jobResource, signedString))))
              }
          }

        case event: BasicItemEvent.ForController =>
          event match {
            case event @ ItemAttachedStateChanged(id, agentPath, attachedState) =>
              id match {
                case id: OrderWatchPath =>
                  allOrderWatchesState.updateAttachedState(id, agentPath, attachedState)
                    .map(o => copy(
                      allOrderWatchesState = o))

                case _: VersionedItemId_ =>
                  for (repo <- repo.applyBasicItemEvent(event)) yield
                    copy(repo = repo)

                case id: SignableSimpleItemPath =>
                  // TODO Code is similar to ControllerStateBuidler
                  attachedState match {
                    case attachedState: NotDetached =>
                      Right(copy(
                        itemToAgentToAttachedState = itemToAgentToAttachedState +
                          (id ->
                            (itemToAgentToAttachedState.getOrElse(id, Map.empty) +
                              (agentPath -> attachedState)))))

                    case Detached =>
                      Right(copy(itemToAgentToAttachedState = {
                        val updated = itemToAgentToAttachedState.getOrElse(id, Map.empty) - agentPath
                        if (updated.isEmpty)
                          itemToAgentToAttachedState - id
                        else
                          itemToAgentToAttachedState + (id -> updated)
                      }))
                  }

                case _ =>
                  eventNotApplicable(keyedEvent)
              }

            case ItemDeletionMarked(itemId) =>
              itemId match {
                case id: OrderWatchPath =>
                  allOrderWatchesState.markAsDeleted(id)
                    .map(o => copy(
                      allOrderWatchesState = o))

                case _ =>
                  Left(Problem(s"A '${itemId.companion.itemTypeName}' is not deletable (in this version)"))  // TODO
              }

            case ItemDestroyed(id) =>
              id match {
                case id: OrderWatchPath =>
                  Right(copy(
                    allOrderWatchesState = allOrderWatchesState.removeOrderWatch(id)))

                case _ =>
                  Left(Problem(s"A '${id.companion.itemTypeName}' is not deletable (in this version)"))  // TODO
              }
          }
      }

    case KeyedEvent(_: NoKey, event: VersionedEvent) =>
      for (o <- repo.applyEvent(event)) yield
        copy(repo = o)

    case KeyedEvent(agentPath: AgentPath, event: AgentRefStateEvent) =>
      for {
        agentRefState <- pathToAgentRefState.checked(agentPath)
        agentRefState <- agentRefState.applyEvent(event)
      } yield copy(
        pathToAgentRefState = pathToAgentRefState + (agentPath -> agentRefState))

    case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
      event match {
        case orderAdded: OrderAdded =>
          idToOrder.checkNoDuplicate(orderId) >>
            allOrderWatchesState.onOrderAdded(orderId <-: orderAdded)
              .map(updated => copy(
                idToOrder = idToOrder + (orderId -> Order.fromOrderAdded(orderId, orderAdded)),
          allOrderWatchesState = updated))

        case event: OrderCoreEvent =>
          for {
            previousOrder <- idToOrder.checked(orderId)
            updatedOrder <- previousOrder.applyEvent(event)
            updatedIdToOrder = idToOrder + (updatedOrder.id -> updatedOrder)
            updatedControllerState <- event match {
              case event: OrderForked =>
                Right(copy(
                  idToOrder = updatedIdToOrder
                    ++ previousOrder.newForkedOrders(event).map(childOrder => childOrder.id -> childOrder)))

              case event: OrderJoined =>
                previousOrder.state match {
                  case forked: Order.Forked =>
                    Right(copy(
                      idToOrder = updatedIdToOrder -- forked.childOrderIds))

                  case awaiting: Order.Awaiting =>
                    // Offered order is being kept ???
                    //Right(idToOrder - awaiting.offeredOrderId)
                    Right(this)

                  case state =>
                    Left(Problem(s"For event $event, $orderId must be in state Forked or Awaiting, not: $state"))
                }

              case event: OrderOffered =>
                val offered = previousOrder.newOfferedOrder(event)
                for (_ <- idToOrder.checkNoDuplicate(offered.id)) yield
                  copy(
                    idToOrder = updatedIdToOrder + (offered.id -> offered))

              case event: OrderLockEvent =>
                event.lockPaths
                  .toList
                  .traverse(lockPath => pathToLockState(lockPath).applyEvent(orderId <-: event))
                  .map(lockStates =>
                    copy(
                      idToOrder = updatedIdToOrder,
                      pathToLockState = pathToLockState ++ (lockStates.map(o => o.lock.path -> o))))

              case OrderRemoveMarked =>
                previousOrder.externalOrderKey match {
                  case None =>
                    Right(copy(idToOrder = updatedIdToOrder))
                  case Some(externalOrderKey) =>
                    allOrderWatchesState.onOrderEvent(externalOrderKey, orderId <-: OrderRemoveMarked)
                      .map(o => copy(
                        idToOrder = updatedIdToOrder,
                        allOrderWatchesState = o))
                }

              case OrderRemoved =>
                previousOrder.externalOrderKey match {
                  case None =>
                    Right(copy(idToOrder = idToOrder - orderId))
                  case Some(externalOrderKey) =>
                    allOrderWatchesState
                      .onOrderEvent(externalOrderKey, orderId <-: OrderRemoved)
                      .map(o => copy(
                        idToOrder = idToOrder - orderId,
                        allOrderWatchesState = o))
                }

              case _ => Right(copy(idToOrder = updatedIdToOrder))
            }
          } yield updatedControllerState

        case _: OrderStdWritten =>
          Right(this)
      }

    case KeyedEvent(orderWatchPath: OrderWatchPath, event: OrderWatchEvent) =>
      allOrderWatchesState
        .onOrderWatchEvent(orderWatchPath <-: event)
        .map(o => copy(allOrderWatchesState = o))

    case KeyedEvent(_, _: ControllerShutDown) =>
      Right(this)

    case KeyedEvent(_, ControllerTestEvent) =>
      Right(this)

    case _ => applyStandardEvent(keyedEvent)
  }

  def itemToAttachedState(itemKey: InventoryItemKey, itemRevision: Option[ItemRevision], agentPath: AgentPath)
  : ItemAttachedState =
    itemKey match {
      case _: UnsignedSimpleItemPath =>
        Detached

      case path: SignableSimpleItemPath =>
        itemToAgentToAttachedState
          .get(path)
          .flatMap(_.get(agentPath))
          .map {
            case a @ (Attachable | Attached(`itemRevision`) | Detachable) => a
            case Attached(_) => Detached
          }
          .getOrElse(Detached)

      case id: VersionedItemId_ =>
        repo.attachedState(id, agentPath)
    }

  lazy val pathToSimpleItem: MapView[SimpleItemPath, SimpleItem] =
    new MapView[SimpleItemPath, SimpleItem] {
      def get(path: SimpleItemPath): Option[SimpleItem] =
        path match {
          case path: AgentPath => pathToAgentRefState.get(path).map(_.item)
          case path: LockPath => pathToLockState.get(path).map(_.item)
          case path: OrderWatchPath => allOrderWatchesState.pathToOrderWatchState.get(path).map(_.item)
          case path: JobResourcePath => idToSignedSimpleItem.get(path).map(_.value.asInstanceOf[JobResource])
          case path =>
            scribe.error(s"pathToSimpleItem: Unexpected SimpleItemPath: $path")
            None
        }

      def iterator: Iterator[(SimpleItemPath, SimpleItem)] =
        Iterator(pathToAgentRefState, pathToLockState, allOrderWatchesState.pathToOrderWatchState)
          .flatMap(_.view.mapValues(_.item).iterator)
    }

  override def toString = s"ControllerState(${EventId.toString(eventId)} ${idToOrder.size} orders, " +
      s"Repo(${repo.currentVersionSize} objects, ...))"
}

object ControllerState extends JournaledState.Companion[ControllerState]
{
  val Undefined = ControllerState(
    EventId.BeforeFirst,
    JournaledState.Standards.empty,
    ControllerMetaState.Undefined,
    Map.empty,
    Map.empty,
    AllOrderWatchesState.empty,
    Repo.empty,
    Map.empty,
    Map.empty,
    Map.empty)

  val empty = Undefined

  def newBuilder() = new ControllerStateBuilder

  protected val InventoryItems = Seq[InventoryItem.Companion_](
    AgentRef, Lock, FileWatch, JobResource, Workflow)

  private[controller] final case class ItemAttachedStateSnapshot(
    key: InventoryItemKey,
    agentToAttachedState: Map[AgentPath, ItemAttachedState.NotDetached])

  lazy val snapshotObjectJsonCodec: TypedJsonCodec[Any] =
    TypedJsonCodec("ControllerState.Snapshot",
      Subtype[JournalHeader],
      Subtype[SnapshotMeta],
      Subtype[JournalState],
      Subtype(deriveCodec[ClusterStateSnapshot]),
      Subtype(deriveCodec[ControllerMetaState]),
      Subtype[AgentRefState],
      Subtype[LockState],
      Subtype[VersionedEvent],  // These events describe complete objects
      Subtype[InventoryItemEvent],  // For Repo and SignedItemAdded
      Subtype(deriveCodec[ItemAttachedStateSnapshot]),
      Subtype[Order[Order.State]],
      Subtype[OrderWatchState.Snapshot])

  implicit lazy val keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    KeyedEventTypedJsonCodec("ControlllerState.Event",
      KeyedSubtype[JournalEvent],
      KeyedSubtype[InventoryItemEvent],
      KeyedSubtype[VersionedEvent],
      KeyedSubtype[ControllerEvent],
      KeyedSubtype[ClusterEvent],
      KeyedSubtype[AgentRefStateEvent],
      KeyedSubtype[OrderWatchEvent],
      KeyedSubtype[OrderEvent])

  object implicits {
    implicit val snapshotObjectJsonCodec = ControllerState.snapshotObjectJsonCodec
  }
}
