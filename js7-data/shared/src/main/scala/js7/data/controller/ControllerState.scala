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
import js7.data.agent.{AgentId, AgentRef, AgentRefState, AgentRefStateEvent}
import js7.data.cluster.{ClusterEvent, ClusterStateSnapshot}
import js7.data.controller.ControllerEvent.{ControllerShutDown, ControllerTestEvent}
import js7.data.controller.ControllerState.ItemAttachedStateSnapshot
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{Event, EventId, JournalEvent, JournalHeader, JournalState, JournaledState, KeyedEvent, KeyedEventTypedJsonCodec, SnapshotMeta}
import js7.data.item.CommonItemEvent.{ItemAttachedStateChanged, ItemDeletionMarked, ItemDestroyed}
import js7.data.item.ItemAttachedState.{Detached, NotDetached}
import js7.data.item.SignedItemEvent.{SignedItemAdded, SignedItemChanged}
import js7.data.item.UnsignedSimpleItemEvent.{SimpleItemAdded, SimpleItemChanged}
import js7.data.item.{CommonItemEvent, InventoryItem, InventoryItemEvent, InventoryItemId, ItemAttachedState, Repo, SignableSimpleItem, SignableSimpleItemId, SignedItemEvent, SimpleItem, SimpleItemId, UnsignedSimpleItemEvent, VersionedEvent, VersionedItemId_}
import js7.data.job.{JobResource, JobResourceId}
import js7.data.lock.{Lock, LockId, LockState}
import js7.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderForked, OrderJoined, OrderLockEvent, OrderOffered, OrderRemoveMarked, OrderRemoved, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{AllOrderWatchesState, FileWatch, OrderWatch, OrderWatchEvent, OrderWatchId, OrderWatchState}
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
  idToAgentRefState: Map[AgentId, AgentRefState],
  idToLockState: Map[LockId, LockState],
  allOrderWatchesState: AllOrderWatchesState,
  repo: Repo,
  idToSignedSimpleItem: Map[SignableSimpleItemId, Signed[SignableSimpleItem]],
  itemIdToAgentToAttachedState: Map[InventoryItemId, Map[AgentId, ItemAttachedState.NotDetached]],
  idToOrder: Map[OrderId, Order[Order.State]])
extends JournaledState[ControllerState]
{
  def estimatedSnapshotSize: Int =
    1 +
    standards.snapshotSize +
    controllerMetaState.isDefined.toInt +
    repo.estimatedEventCount +
    idToAgentRefState.size +
    idToLockState.size +
    allOrderWatchesState.estimatedSnapshotSize +
    idToSignedSimpleItem.size +
    itemIdToAgentToAttachedState.values.view.map(_.size).sum +
    idToOrder.size

  def toSnapshotObservable: Observable[Any] =
    Observable.pure(SnapshotEventId(eventId)) ++
    standards.toSnapshotObservable ++
    Observable.fromIterable(controllerMetaState.isDefined ? controllerMetaState) ++
    Observable.fromIterable(repo.toEvents) ++
    Observable.fromIterable(idToAgentRefState.values) ++
    Observable.fromIterable(idToLockState.values) ++
    allOrderWatchesState.toSnapshot ++
    Observable.fromIterable(idToSignedSimpleItem.values)
      .map(SignedItemAdded(_)) ++
    Observable.fromIterable(itemIdToAgentToAttachedState)
      .map(o => ItemAttachedStateSnapshot(o._1, o._2)) ++
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
                  for (o <- idToLockState.insert(lock.id -> LockState(lock))) yield
                    copy(idToLockState = o)

                case agentRef: AgentRef =>
                  for (o <- idToAgentRefState.insert(agentRef.id -> AgentRefState(agentRef)))
                    yield copy(
                      idToAgentRefState = o)

                case orderWatch: OrderWatch =>
                  for (o <- allOrderWatchesState.addOrderWatch(orderWatch)) yield
                    copy(allOrderWatchesState = o)
              }

            case SimpleItemChanged(item) =>
              item match {
                case lock: Lock =>
                  for (lockState <- idToLockState.checked(lock.id))
                    yield copy(
                      idToLockState = idToLockState + (lock.id -> lockState.copy(
                        lock = lock)))

                case agentRef: AgentRef =>
                  for (agentRefState <- idToAgentRefState.checked(agentRef.id))
                    yield copy(
                      idToAgentRefState = idToAgentRefState + (agentRef.id -> agentRefState.copy(
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
                  for (o <- idToSignedSimpleItem.insert(jobResource.id -> Signed(jobResource, signedString))) yield
                    copy(idToSignedSimpleItem = o)
              }

            case SignedItemChanged(Signed(item, signedString)) =>
              item match {
                case jobResource: JobResource =>
                  Right(copy(
                    idToSignedSimpleItem = idToSignedSimpleItem + (jobResource.id -> Signed(jobResource, signedString))))
              }
          }

        case event: CommonItemEvent.ForController =>
          event match {
            case event @ ItemAttachedStateChanged(id, agentId, attachedState) =>
              id match {
                case id: OrderWatchId =>
                  allOrderWatchesState.updateAttachedState(id, agentId, attachedState)
                    .map(o => copy(
                      allOrderWatchesState = o))

                case _: VersionedItemId_ =>
                  for (repo <- repo.applyCommonItemEvent(event)) yield
                    copy(repo = repo)

                case id: SignableSimpleItemId =>
                  // TODO Code is similar to ControllerStateBuidler
                  attachedState match {
                    case attachedState: NotDetached =>
                      Right(copy(
                        itemIdToAgentToAttachedState = itemIdToAgentToAttachedState +
                          (id ->
                            (itemIdToAgentToAttachedState.getOrElse(id, Map.empty) +
                              (agentId -> attachedState)))))

                    case Detached =>
                      Right(copy(itemIdToAgentToAttachedState = {
                        val updated = itemIdToAgentToAttachedState.getOrElse(id, Map.empty) - agentId
                        if (updated.isEmpty)
                          itemIdToAgentToAttachedState - id
                        else
                          itemIdToAgentToAttachedState + (id -> updated)
                      }))
                  }

                case _ =>
                  eventNotApplicable(keyedEvent)
              }

            case ItemDeletionMarked(itemId) =>
              itemId match {
                case id: OrderWatchId =>
                  allOrderWatchesState.markAsDeleted(id)
                    .map(o => copy(
                      allOrderWatchesState = o))

                case _ =>
                  Left(Problem(s"A '${itemId.companion.itemTypeName}' is not deletable (in this version)"))  // TODO
              }

            case ItemDestroyed(id) =>
              id match {
                case id: OrderWatchId =>
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

    case KeyedEvent(agentId: AgentId, event: AgentRefStateEvent) =>
      idToAgentRefState.checked(agentId)
        .flatMap(agentRefState =>
          agentRefState.applyEvent(event)
            .map(updated => copy(
              idToAgentRefState = idToAgentRefState + (agentId -> updated))))

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
                event.lockIds
                  .toList
                  .traverse(lockId => idToLockState(lockId).applyEvent(orderId <-: event))
                  .map(lockStates =>
                    copy(
                      idToOrder = updatedIdToOrder,
                      idToLockState = idToLockState ++ (lockStates.map(o => o.lock.id -> o))))

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

    case KeyedEvent(orderWatchId: OrderWatchId, event: OrderWatchEvent) =>
      allOrderWatchesState
        .onOrderWatchEvent(orderWatchId <-: event)
        .map(o => copy(allOrderWatchesState = o))

    case KeyedEvent(_, _: ControllerShutDown) =>
      Right(this)

    case KeyedEvent(_, ControllerTestEvent) =>
      Right(this)

    case _ => applyStandardEvent(keyedEvent)
  }

  def itemIdToAttachedState(itemId: InventoryItemId, agentId: AgentId): ItemAttachedState =
    itemId match {
      case itemId: VersionedItemId_ =>
        repo.attachedState(itemId, agentId)

      case itemId: SignableSimpleItemId =>
        itemIdToAgentToAttachedState.get(itemId)
          .flatMap(_.get(agentId))
          .getOrElse(Detached)
    }

  lazy val idToSimpleItem: MapView[SimpleItemId, SimpleItem] =
    new MapView[SimpleItemId, SimpleItem] {
      def get(itemId: SimpleItemId): Option[SimpleItem] =
        itemId match {
          case id: AgentId => idToAgentRefState.get(id).map(_.item)
          case id: LockId => idToLockState.get(id).map(_.item)
          case id: OrderWatchId => allOrderWatchesState.idToOrderWatchState.get(id).map(_.item)
          case id: JobResourceId => idToSignedSimpleItem.get(id).map(_.value.asInstanceOf[JobResource])
          case id =>
            scribe.error(s"idToSimpleItem: Unexpected SimpleItemId: $id")
            None
        }

      def iterator: Iterator[(SimpleItemId, SimpleItem)] =
        Iterator(idToAgentRefState, idToLockState, allOrderWatchesState.idToOrderWatchState)
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
    itemId: InventoryItemId,
    agentToAttachedState: Map[AgentId, ItemAttachedState.NotDetached])

  lazy val snapshotObjectJsonCodec: TypedJsonCodec[Any] =
    TypedJsonCodec[Any](
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
    KeyedEventTypedJsonCodec[Event](
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
