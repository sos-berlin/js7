package js7.data.controller

import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import js7.base.circeutils.CirceCodec
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.{AgentId, AgentRef, AgentRefState, AgentRefStateEvent}
import js7.data.cluster.{ClusterEvent, ClusterStateSnapshot}
import js7.data.controller.ControllerEvent.{ControllerShutDown, ControllerTestEvent}
import js7.data.controller.ControllerState.generic.itemPathJsonCodec
import js7.data.controller.ControllerState.itemPathCompanions
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{Event, EventId, JournalEvent, JournalHeader, JournalState, JournaledState, KeyedEvent, KeyedEventTypedJsonCodec, SnapshotMeta}
import js7.data.item.SimpleItemEvent.{SimpleItemAdded, SimpleItemAttachedStateChanged, SimpleItemChanged, SimpleItemDeleted}
import js7.data.item.{ItemPath, Repo, SimpleItem, SimpleItemEvent, SimpleItemId, SimpleItemState, VersionedEvent, VersionedItem}
import js7.data.lock.{Lock, LockId, LockState}
import js7.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderForked, OrderJoined, OrderLockEvent, OrderOffered, OrderRemoveMarked, OrderRemoved, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{AllOrderWatchesState, FileWatch, OrderWatch, OrderWatchEvent, OrderWatchId, OrderWatchState}
import js7.data.workflow.{Workflow, WorkflowPath}
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
    idToOrder.size

  def toSnapshotObservable: Observable[Any] =
    Observable.pure(SnapshotEventId(eventId)) ++
    standards.toSnapshotObservable ++
    Observable.fromIterable(controllerMetaState.isDefined ? controllerMetaState) ++
    Observable.fromIterable(repo.eventsFor(itemPathCompanions)) ++
    Observable.fromIterable(idToAgentRefState.values) ++
    Observable.fromIterable(idToLockState.values) ++
    allOrderWatchesState.toSnapshot ++
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

    case KeyedEvent(_: NoKey, event: SimpleItemEvent) =>
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
              for (o <- allOrderWatchesState.changeOrderWatch(orderWatch)) yield
                copy(allOrderWatchesState = o)
          }

        case SimpleItemDeleted(itemId) =>
          Left(Problem(s"A '${itemId.companion.name}' is not deletable (in this version)"))  // TODO

        case SimpleItemAttachedStateChanged(id, agentId, attachedState) =>
          id match {
            case orderWatchId: OrderWatchId =>
              allOrderWatchesState.updatedAttachedState(orderWatchId, agentId, attachedState)
                .map(o => copy(allOrderWatchesState = o))

            case _ =>
              eventNotApplicable(keyedEvent)
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

  def idToItem: MapView[SimpleItemId, SimpleItem] =
    idToItemState.mapValues(_.item)

  private def idToItemState: MapView[SimpleItemId, SimpleItemState] =
    new MapView[SimpleItemId, SimpleItemState] {
      def get(itemId: SimpleItemId): Option[SimpleItemState] = {
        itemId match {
          case id: AgentId => idToAgentRefState.get(id)
          case id: LockId => idToLockState.get(id)
          case id: OrderWatchId => allOrderWatchesState.idToOrderWatchState.get(id)
        }
      }

      def iterator: Iterator[(SimpleItemId, SimpleItemState)] =
        Iterator(idToAgentRefState, idToLockState, allOrderWatchesState.idToOrderWatchState)
          .flatMap(_.view.iterator)
    }

  override def toString =
    s"ControllerState(${EventId.toString(eventId)} ${idToOrder.size} orders, Repo(${repo.currentVersionSize} objects, ...))"
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
    Map.empty)

  val empty = Undefined

  private val simpleItemCompanions = Seq[SimpleItem.Companion](
    AgentRef, Lock, FileWatch)

  private val itemPathCompanions = Set[ItemPath.AnyCompanion](
    WorkflowPath)

  implicit val simpleItemJsonCodec: TypedJsonCodec[SimpleItem] =
    TypedJsonCodec(simpleItemCompanions.map(_.subtype): _*)

  implicit val simpleItemEventJsonCodec = SimpleItemEvent.jsonCodec(simpleItemCompanions)

  def newBuilder() = new ControllerStateBuilder

  trait GenericImplicits {
    implicit val simpleItemIdJsonCodec: CirceCodec[SimpleItemId] =
      SimpleItemId.jsonCodec(simpleItemCompanions.map(_.idCompanion))

    implicit val itemPathJsonCodec: CirceCodec[ItemPath] =
      ItemPath.jsonCodec(itemPathCompanions)
  }
  val generic = new GenericImplicits {}

  implicit val versionedItemJsonCodec: TypedJsonCodec[VersionedItem] = TypedJsonCodec(
    Subtype(Workflow.jsonEncoder, Workflow.topJsonDecoder))

  val snapshotObjectJsonCodec: TypedJsonCodec[Any] =
    TypedJsonCodec[Any](
      Subtype[JournalHeader],
      Subtype[SnapshotMeta],
      Subtype[JournalState],
      Subtype(deriveCodec[ClusterStateSnapshot]),
      Subtype(deriveCodec[ControllerMetaState]),
      Subtype[AgentRefState],
      Subtype[LockState],
      Subtype[VersionedEvent],  // These events describe complete objects
      Subtype[Order[Order.State]],
      Subtype[OrderWatchState.Snapshot])

  implicit val keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    KeyedEventTypedJsonCodec[Event](
      KeyedSubtype[JournalEvent],
      KeyedSubtype[SimpleItemEvent],
      KeyedSubtype[VersionedEvent],
      KeyedSubtype[ControllerEvent],
      KeyedSubtype[ClusterEvent],
      KeyedSubtype[AgentRefStateEvent],
      KeyedSubtype[OrderWatchEvent],
      KeyedSubtype[OrderEvent])
}
