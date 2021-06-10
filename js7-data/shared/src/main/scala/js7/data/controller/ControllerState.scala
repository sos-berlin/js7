package js7.data.controller

import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.Signed
import js7.base.problem.Checked.{CheckedOption, RichCheckedIterable}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax._
import js7.data.Problems.{ItemIsStillReferencedProblem, MissingReferencedItemProblem}
import js7.data.agent.{AgentPath, AgentRef, AgentRefState, AgentRefStateEvent}
import js7.data.cluster.{ClusterEvent, ClusterStateSnapshot}
import js7.data.controller.ControllerEvent.{ControllerShutDown, ControllerTestEvent}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{Event, EventId, JournalEvent, JournalHeader, JournalState, JournaledState, KeyedEvent, KeyedEventTypedJsonCodec, SnapshotMeta}
import js7.data.item.BasicItemEvent.{ItemAttachedStateChanged, ItemDeleted, ItemDeletionMarked, ItemDetachable}
import js7.data.item.ItemAttachedState.{Attachable, Attached, Detachable, Detached, NotDetached}
import js7.data.item.SignedItemEvent.{SignedItemAdded, SignedItemChanged}
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemChanged}
import js7.data.item.{BasicItemEvent, InventoryItem, InventoryItemEvent, InventoryItemKey, InventoryItemPath, ItemAttachedState, ItemRevision, Repo, SignableItem, SignableItemKey, SignableSimpleItem, SignableSimpleItemPath, SignedItemEvent, SimpleItem, SimpleItemPath, UnsignedSimpleItem, UnsignedSimpleItemEvent, UnsignedSimpleItemPath, VersionedEvent, VersionedItemId_, VersionedItemPath}
import js7.data.job.JobResource
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderDeleted, OrderDeletionMarked, OrderForked, OrderJoined, OrderLockEvent, OrderOffered, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{AllOrderWatchesState, FileWatch, OrderWatch, OrderWatchEvent, OrderWatchPath, OrderWatchState}
import js7.data.workflow.{Workflow, WorkflowId}
import monix.reactive.Observable
import scala.collection.{MapView, View}
import scala.util.chaining.scalaUtilChainingOps

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
  /** Used for OrderWatch to allow to attach it from Agent. */
  deletionMarkedItems: Set[InventoryItemKey],
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
    deletionMarkedItems.size +
    idToOrder.size

  def toSnapshotObservable: Observable[Any] =
    Observable(
      Observable.pure(SnapshotEventId(eventId)),
      standards.toSnapshotObservable,
      Observable.fromIterable(controllerMetaState.isDefined ? controllerMetaState),
      Observable.fromIterable(pathToAgentRefState.values),
      Observable.fromIterable(pathToLockState.values),
      allOrderWatchesState.toSnapshot/*TODO Separate Item from its state?*/,
      Observable.fromIterable(idToSignedSimpleItem.values).map(SignedItemAdded(_)),
      Observable.fromIterable(repo.toEvents),
      Observable.fromIterable(itemToAgentToAttachedState
        .to(View)
        .flatMap { case (key, agentToAttached) =>
          agentToAttached.map { case (agentPath, attachedState) =>
            ItemAttachedStateChanged(key, agentPath, attachedState)
          }
        }),
      Observable.fromIterable(deletionMarkedItems.map(ItemDeletionMarked(_))),
      Observable.fromIterable(idToOrder.values)
    ).flatten

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
            case UnsignedSimpleItemAdded(item) =>
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

            case UnsignedSimpleItemChanged(item) =>
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
            case ItemAttachedStateChanged(itemKey, agentPath, attachedState) =>
              attachedState match {
                case attachedState: NotDetached =>
                  Right(copy(
                    itemToAgentToAttachedState = itemToAgentToAttachedState +
                      (itemKey ->
                        (itemToAgentToAttachedState.getOrElse(itemKey, Map.empty) +
                          (agentPath -> attachedState)))))

                case Detached =>
                  for {
                    agentToAttachedState <- itemToAgentToAttachedState.checked(itemKey)
                    _ <- agentToAttachedState.checked(agentPath)
                  } yield
                    copy(itemToAgentToAttachedState = {
                      val updated = agentToAttachedState - agentPath
                      if (updated.isEmpty)
                        itemToAgentToAttachedState - itemKey
                      else
                        itemToAgentToAttachedState + (itemKey -> updated)
                    })
              }

            case ItemDeletionMarked(itemKey) =>
              Right(copy(
                deletionMarkedItems = deletionMarkedItems + itemKey))

            case ItemDeleted(itemKey) =>
              itemKey match {
                case id: VersionedItemId_ =>
                  for (repo <- repo.deleteItem(id)) yield
                    copy(
                      repo = repo)

                case lockPath: LockPath =>
                  Right(copy(
                    pathToLockState = pathToLockState - lockPath))

                case agentPath: AgentPath =>
                  Right(copy(
                    pathToAgentRefState = pathToAgentRefState - agentPath))

                case path: OrderWatchPath =>
                  Right(copy(
                    deletionMarkedItems = deletionMarkedItems - path,
                    itemToAgentToAttachedState = itemToAgentToAttachedState - path,
                    allOrderWatchesState = allOrderWatchesState.removeOrderWatch(path)))

                case _ =>
                  Left(Problem(s"A '${itemKey.companion.itemTypeName}' is not deletable"))
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

              case OrderDeletionMarked =>
                previousOrder.externalOrderKey match {
                  case None =>
                    Right(copy(idToOrder = updatedIdToOrder))
                  case Some(externalOrderKey) =>
                    allOrderWatchesState.onOrderEvent(externalOrderKey, orderId <-: OrderDeletionMarked)
                      .map(o => copy(
                        idToOrder = updatedIdToOrder,
                        allOrderWatchesState = o))
                }

              case OrderDeleted =>
                previousOrder.externalOrderKey match {
                  case None =>
                    Right(copy(idToOrder = idToOrder - orderId))
                  case Some(externalOrderKey) =>
                    allOrderWatchesState
                      .onOrderEvent(externalOrderKey, orderId <-: OrderDeleted)
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

  private[controller] def checkAddedOrChangedItems(itemKeys: Iterable[InventoryItemKey]): Checked[Unit] =
    itemKeys
      .flatMap(itemKey =>
        keyToItem.checked(itemKey)
          .flatTraverse(_
            .referencedItemPaths
            .map(path => pathToItem
              .get(path)
              .toChecked(MissingReferencedItemProblem(itemKey, referencedItemKey = path)))
            .toVector))
      .combineProblems
      .rightAs(())

  private[controller] def checkRemovedVersionedItems(deletedPaths: Iterable[VersionedItemPath])
  : Checked[Unit] =
    deletedPaths.view
      .map(checkVersionedItemIsDeletable)
      .combineProblems
      .rightAs(())

  private def checkVersionedItemIsDeletable(path: VersionedItemPath): Checked[Unit] =
    referencingItemKeys(path)
      .map(ItemIsStillReferencedProblem(path, _))
      .reduceLeftOption(Problem.combine)
      .toLeft(())

  private[controller] def checkDeletedSimpleItems(deletedPaths: Iterable[SimpleItemPath])
  : Checked[Unit] =
    deletedPaths.view
      .map(checkSimpleItemIsDeletable)
      .combineProblems
      .rightAs(())

  private def checkSimpleItemIsDeletable(path: SimpleItemPath): Checked[Unit] =
    referencingItemKeys(path)
      .map(ItemIsStillReferencedProblem(path, _))
      .reduceLeftOption(Problem.combine)
      .toLeft(())

  private[controller] def detach(itemKey: InventoryItemKey): View[ItemDetachable] =
    itemToAgentToAttachedState
      .getOrElse(itemKey, Map.empty)
      .view
      .flatMap {
        case (agentPath, notDetached) => toDetachEvent(itemKey, agentPath, notDetached)
      }

  private def toDetachEvent(itemKey: InventoryItemKey, agentPath: AgentPath, notDetached: NotDetached)
  : Option[ItemDetachable] =
    notDetached match {
      case Attached(_) => Some(ItemDetachable(itemKey, agentPath))
      case _ => None
    }

  private[controller] def isReferenced(path: InventoryItemPath): Boolean =
    pathToReferencingItemKeys contains path

  private def referencingItemKeys(path: InventoryItemPath): View[InventoryItemKey] =
    pathToReferencingItemKeys.get(path).view.flatten

  // Slow ???
  private[controller] lazy val pathToReferencingItemKeys: Map[InventoryItemPath, Seq[InventoryItemKey]] =
    items
      .flatMap(item => item.referencedItemPaths.map(item.key -> _))
      .groupMap(_._2)(_._1)
      .view
      .mapValues(_.toVector)
      .toMap
      .tap(o => scribe.trace(s"${items.size} items => pathToReferencingItemKeys size=${o.size}"))

  private[controller] def isObsoleteItem(itemId: VersionedItemId_) =
    !repo.isCurrentItem(itemId) && !isInUse(itemId)

  private[controller] def isInUse(itemId: VersionedItemId_) =
    itemId match {
      case WorkflowId.as(workflowId) => isWorkflowUsedByOrders(workflowId)
      case _ => true
    }

  // Slow ???
  private[controller] lazy val isWorkflowUsedByOrders: Set[WorkflowId] =
    idToOrder.values.view.map(_.workflowId).toSet
      .tap(o => scribe.trace(s"${idToOrder.size} orders => isWorkflowUsedByOrders size=${o.size}"))

  def itemToAttachedState(itemKey: InventoryItemKey, itemRevision: Option[ItemRevision], agentPath: AgentPath)
  : ItemAttachedState =
    itemToAgentToAttachedState
      .get(itemKey)
      .flatMap(_.get(agentPath))
      .map {
        case a @ (Attachable | Attached(`itemRevision`) | Detachable) => a
        case Attached(_) => Detached
      }
      .getOrElse(Detached)

  lazy val keyToItem: MapView[InventoryItemKey, InventoryItem] =
    new MapView[InventoryItemKey, InventoryItem] {
      def get(itemKey: InventoryItemKey): Option[InventoryItem] =
        itemKey match {
          case id: VersionedItemId_ => repo.anyIdToItem(id).toOption
          case path: SimpleItemPath => pathToSimpleItem.get(path)
        }

      def iterator: Iterator[(InventoryItemKey, InventoryItem)] =
        pathToSimpleItem.iterator ++
          repo.items.map(item => item.id -> item)
    }

  private lazy val pathToItem: MapView[InventoryItemPath, InventoryItem] =
    new MapView[InventoryItemPath, InventoryItem] {
      def get(path: InventoryItemPath): Option[InventoryItem] = {
        path match {
          case path: SimpleItemPath => pathToSimpleItem.get(path)
          case path: VersionedItemPath => repo.pathToItem(path).toOption
        }
      }

      def iterator: Iterator[(InventoryItemPath, InventoryItem)] =
        pathToSimpleItem.iterator ++ repo.currentItems.iterator.map(o => o.path -> o)
    }

  lazy val pathToSimpleItem: MapView[SimpleItemPath, SimpleItem] =
    new MapView[SimpleItemPath, SimpleItem] {
      def get(path: SimpleItemPath): Option[SimpleItem] =
        keyToItemFunc(path) collect { case o: SimpleItem => o }

      def iterator: Iterator[(SimpleItemPath, SimpleItem)] =
        simpleItems.view.map(item => item.key -> item).iterator
    }

  def items: View[InventoryItem] =
    simpleItems ++ repo.items

  def simpleItems: View[SimpleItem] =
    unsignedSimpleItems ++ idToSignedSimpleItem.values.view.map(_.value)

  private def unsignedSimpleItems: View[UnsignedSimpleItem] =
    (pathToAgentRefState.view ++
      pathToLockState ++
      allOrderWatchesState.pathToOrderWatchState
    ).map(_._2.item)

  lazy val pathToUnsignedSimpleItem: MapView[UnsignedSimpleItemPath, UnsignedSimpleItem] =
    new MapView[UnsignedSimpleItemPath, UnsignedSimpleItem] {
      def get(path: UnsignedSimpleItemPath): Option[UnsignedSimpleItem] =
        keyToItemFunc(path) collect { case o: UnsignedSimpleItem => o }

      def iterator: Iterator[(UnsignedSimpleItemPath, UnsignedSimpleItem)] =
        unsignedSimpleItems.map(item => item.path -> item).iterator
    }

  lazy val keyToSignedItem: MapView[SignableItemKey, Signed[SignableItem]] =
    new MapView[SignableItemKey, Signed[SignableItem]] {
      def get(itemKey: SignableItemKey): Option[Signed[SignableItem]] =
        itemKey match {
          case id: VersionedItemId_ => repo.anyIdToSigned(id).toOption
          case path: SignableSimpleItemPath =>
            idToSignedSimpleItem.get(path)
        }

      def iterator: Iterator[(SignableItemKey, Signed[SignableItem])] =
        Iterator(
          repo.pathToVersionToSignedItems.values.view
            .flatMap(_
              .flatMap(_.maybeSignedItem)
              .map(signed => signed.value.key -> signed)),
          idToSignedSimpleItem
        ).flatten
    }

  private def keyToItemFunc(itemKey: InventoryItemKey): Option[InventoryItem] =
    itemKey match {
      case id: VersionedItemId_ => repo.anyIdToSigned(id).toOption.map(_.value)
      case path: AgentPath => pathToAgentRefState.get(path).map(_.item)
      case path: LockPath => pathToLockState.get(path).map(_.item)
      case path: OrderWatchPath => allOrderWatchesState.pathToOrderWatchState.get(path).map(_.item)
      case path: SignableSimpleItemPath => idToSignedSimpleItem.get(path).map(_.value)
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
    Set.empty,
    Map.empty)

  val empty = Undefined

  def newBuilder() = new ControllerStateBuilder

  protected val InventoryItems = Seq[InventoryItem.Companion_](
    AgentRef, Lock, FileWatch, JobResource, Workflow)

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
      Subtype[OrderWatchState.Snapshot],
      Subtype[Order[Order.State]])

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
