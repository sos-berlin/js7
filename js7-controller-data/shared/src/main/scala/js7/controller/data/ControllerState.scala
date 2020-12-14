package js7.controller.data

import cats.instances.list._
import cats.syntax.traverse._
import js7.base.circeutils.CirceCodec
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax._
import js7.controller.data.ControllerState.generic.itemPathJsonCodec
import js7.controller.data.ControllerState.itemPathCompanions
import js7.controller.data.agent.AgentRefState
import js7.controller.data.events.AgentRefStateEvent.AgentRegisteredController
import js7.controller.data.events.ControllerEvent.{ControllerShutDown, ControllerTestEvent}
import js7.controller.data.events.{AgentRefStateEvent, ControllerEvent}
import js7.data.agent.{AgentId, AgentRef}
import js7.data.cluster.{ClusterEvent, ClusterStateSnapshot}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{Event, EventId, JournalEvent, JournalHeader, JournalState, JournaledState, KeyedEvent, KeyedEventTypedJsonCodec, SnapshotMeta}
import js7.data.item.SimpleItemEvent.{SimpleItemAdded, SimpleItemChanged, SimpleItemDeleted}
import js7.data.item.{ItemPath, Repo, RepoEvent, SimpleItem, SimpleItemEvent, SimpleItemId, VersionedItem}
import js7.data.lock.{Lock, LockId, LockState}
import js7.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderForked, OrderJoined, OrderLockEvent, OrderOffered, OrderRemoved, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
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
  //simpleItemInventory: SimpleItemInventory,
  repo: Repo,
  idToOrder: Map[OrderId, Order[Order.State]])
extends JournaledState[ControllerState]
{
  def estimatedSnapshotSize: Int =
    2 +
    standards.snapshotSize +
    repo.estimatedEventCount +
    idToAgentRefState.size +
    idToLockState.size +
    idToOrder.size

  def toSnapshotObservable: Observable[Any] =
    Observable.pure(SnapshotEventId(eventId)) ++
    standards.toSnapshotObservable ++
    Observable.fromIterable(controllerMetaState.isDefined ? controllerMetaState) ++
    Observable.fromIterable(repo.eventsFor(itemPathCompanions)) ++
    Observable.fromIterable(idToAgentRefState.values) ++
    Observable.fromIterable(idToLockState.values) ++
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
        case SimpleItemAdded(lock: Lock) =>
          for (o <- idToLockState.insert(lock.id -> LockState(lock))) yield
            copy(idToLockState = o)

        case SimpleItemChanged(lock: Lock) =>
          for (lockState <- idToLockState.checked(lock.id))
            yield copy(
              idToLockState = idToLockState + (lock.id -> lockState.copy(
                lock = lock)))

        case SimpleItemDeleted(_: LockId) =>
          Left(Problem("Locks are not deletable (in this version)"))  // TODO

        case SimpleItemAdded(agentRef: AgentRef) =>
          for (o <- idToAgentRefState.insert(agentRef.id -> AgentRefState(agentRef)))
            yield copy(
              idToAgentRefState = o)

        case SimpleItemChanged(agentRef: AgentRef) =>
          for (agentRefState <- idToAgentRefState.checked(agentRef.id))
            yield copy(
              idToAgentRefState = idToAgentRefState + (agentRef.id -> agentRefState.copy(
                agentRef = agentRef)))

        case SimpleItemDeleted(_: AgentId) =>
          Left(Problem("AgentRefs are not deletable (in this version)"))  // TODO
      }

    case KeyedEvent(_: NoKey, event: RepoEvent) =>
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
        case event: OrderAdded =>
          idToOrder.checkNoDuplicate(orderId).map(_ =>
            copy(idToOrder = idToOrder + (orderId -> Order.fromOrderAdded(orderId, event))))

        case OrderRemoved =>
          Right(copy(idToOrder = idToOrder - orderId))

        case event: OrderCoreEvent =>
          for {
            previousOrder <- idToOrder.checked(orderId)
            updatedOrder <- previousOrder.update(event)
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

              case _ => Right(copy(idToOrder = updatedIdToOrder))
            }
          } yield updatedControllerState

        case _: OrderStdWritten =>
          Right(this)
      }

    case KeyedEvent(_, _: ControllerShutDown) =>
      Right(this)

    case KeyedEvent(_, ControllerTestEvent) =>
      Right(this)

    case _ => applyStandardEvent(keyedEvent)
  }

  def idToItem: MapView[SimpleItemId, SimpleItem] =
    new MapView[SimpleItemId, SimpleItem] {
      override def get(itemId: SimpleItemId): Option[SimpleItem] =
        itemId match {
          case agentId: AgentId => idToAgentRefState.get(agentId).map(_.agentRef)
          case lockId: LockId => idToLockState.get(lockId).map(_.lock)
        }

      override def iterator: Iterator[(SimpleItemId, SimpleItem)] =
        idToLockState.view.mapValues(_.lock).iterator ++
          idToAgentRefState.view.mapValues(_.agentRef).iterator
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
    Repo.empty,
    Map.empty)

  val empty = Undefined

  def newBuilder() = new ControllerStateBuilder

  val simpleItemCompanions = Seq[SimpleItem.Companion](
    AgentRef, Lock)
  val simpleItemIdCompanions = simpleItemCompanions.map(_.idCompanion)

  //implicit val simpleItemIdJsonEncoder: Encoder[SimpleItemId] = SimpleItemId.jsonEncoder(simpleItemIdCompanions.toKeyedMap(_.name).checked)
  //implicit val simpleItemIdJsonDecoder: Decoder[SimpleItemId] = SimpleItemId.jsonDecoder(simpleItemIdCompanions)
  implicit val simpleItemJsonCodec: TypedJsonCodec[SimpleItem] = TypedJsonCodec(
    Subtype[AgentRef],
    Subtype[Lock])

  implicit val simpleItemEventJsonCodec = SimpleItemEvent.jsonCodec(simpleItemCompanions)

  val itemPathCompanions = Set[ItemPath.AnyCompanion](
    WorkflowPath)

  //implicit val itemPathJsonCodec: CirceCodec[ItemPath] = ItemPath.jsonCodec(itemPathCompanions)

  //implicit val itemPathJsonDecoder: Decoder[ItemPath] = ItemPath.jsonDecoder(itemPathCompanions.toKeyedMap(_.name).checked)
  object generic {
    implicit val simpleItemIdJsonCodec: CirceCodec[SimpleItemId] =
      SimpleItemId.jsonCodec(simpleItemIdCompanions)

    implicit val itemPathJsonCodec: CirceCodec[ItemPath] =
      ItemPath.jsonCodec(itemPathCompanions)
  }

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
      Subtype[RepoEvent],  // These events describe complete objects
      Subtype[AgentRegisteredController],  // These events describe complete objects
      Subtype[Order[Order.State]])

  implicit val keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    KeyedEventTypedJsonCodec[Event](
      KeyedSubtype[JournalEvent],
      KeyedSubtype[SimpleItemEvent],
      KeyedSubtype[RepoEvent],
      KeyedSubtype[ControllerEvent],
      KeyedSubtype[ClusterEvent],
      KeyedSubtype[AgentRefStateEvent],
      KeyedSubtype[OrderEvent])
}
