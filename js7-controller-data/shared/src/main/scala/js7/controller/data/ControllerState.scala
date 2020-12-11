package js7.controller.data

import cats.instances.list._
import cats.syntax.traverse._
import js7.base.circeutils.CirceCodec
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.base.problem.Problems.DuplicateKey
import js7.base.utils.ScalaUtils.syntax._
import js7.controller.data.ControllerState.generic.itemPathJsonCodec
import js7.controller.data.ControllerState.itemPathCompanions
import js7.controller.data.agent.AgentRefState
import js7.controller.data.events.AgentRefStateEvent.AgentRegisteredController
import js7.controller.data.events.ControllerEvent.{ControllerShutDown, ControllerTestEvent}
import js7.controller.data.events.{AgentRefStateEvent, ControllerEvent}
import js7.data.agent.AgentRefEvent.{AgentAdded, AgentUpdated}
import js7.data.agent.{AgentId, AgentRef, AgentRefEvent}
import js7.data.cluster.{ClusterEvent, ClusterStateSnapshot}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{Event, EventId, JournalEvent, JournalHeader, JournalState, JournaledState, KeyedEvent, KeyedEventTypedJsonCodec, SnapshotMeta}
import js7.data.item.{ItemPath, Repo, RepoEvent, VersionedItem}
import js7.data.lock.LockEvent.{LockAdded, LockUpdated}
import js7.data.lock.{Lock, LockEvent, LockId, LockState}
import js7.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderForked, OrderJoined, OrderLockEvent, OrderOffered, OrderRemoved, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import monix.reactive.Observable

/**
  * @author Joacim Zschimmer
  */
final case class ControllerState(
  eventId: EventId,
  standards: JournaledState.Standards,
  controllerMetaState: ControllerMetaState,
  nameToAgent: Map[AgentId, AgentRefState],
  nameToLockState: Map[LockId, LockState],
  repo: Repo,
  idToOrder: Map[OrderId, Order[Order.State]])
extends JournaledState[ControllerState]
{
  def estimatedSnapshotSize: Int =
    2 +
    standards.snapshotSize +
    repo.estimatedEventCount +
    nameToAgent.size +
    nameToLockState.size +
    idToOrder.size

  def toSnapshotObservable: Observable[Any] =
    Observable.pure(SnapshotEventId(eventId)) ++
    standards.toSnapshotObservable ++
    Observable.fromIterable(controllerMetaState.isDefined ? controllerMetaState) ++
    Observable.fromIterable(repo.eventsFor(itemPathCompanions)) ++
    Observable.fromIterable(nameToAgent.values) ++
    Observable.fromIterable(nameToLockState.values) ++
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

    case KeyedEvent(_: NoKey, event: RepoEvent) =>
      for (o <- repo.applyEvent(event)) yield
        copy(repo = o)

    case KeyedEvent(name: AgentId, event: AgentRefEvent) =>
      event match {
        case AgentAdded(uri) =>
          if (nameToAgent contains name)
            Left(DuplicateKey("AgentRef", name))
          else
            Right(copy(
              nameToAgent = nameToAgent + (name -> AgentRefState(AgentRef(name, uri)))))

        case AgentUpdated(uri) =>
          for (agentRefState <- nameToAgent.checked(name)) yield
            copy(
              nameToAgent = nameToAgent + (name -> agentRefState.copy(
                agentRef = agentRefState.agentRef.copy(
                  uri = uri))))
      }

    case KeyedEvent(name: AgentId, event: AgentRefStateEvent) =>
      nameToAgent.checked(name)
        .flatMap(agentRefState =>
          agentRefState.applyEvent(event)
            .map(updated => copy(
              nameToAgent = nameToAgent + (name -> updated))))

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
                  .traverse(lockId => nameToLockState(lockId).applyEvent(orderId <-: event))
                  .map(lockStates =>
                    copy(
                      idToOrder = updatedIdToOrder,
                      nameToLockState = nameToLockState ++ (lockStates.map(o => o.lock.id -> o))))

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

    case KeyedEvent(name: LockId, event: LockEvent) =>
      event match {
        case LockAdded(nonExclusiveLimit) =>
          if (nameToLockState contains name)
            Left(DuplicateKey("Lock", name))
          else
            Right(copy(
              nameToLockState = nameToLockState + (name -> LockState(Lock(name, nonExclusiveLimit)))))

        case LockUpdated(nonExclusiveLimit) =>
          Right(copy(
              nameToLockState = nameToLockState + (name -> LockState(Lock(name, nonExclusiveLimit)))))
      }

    case _ => applyStandardEvent(keyedEvent)
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

  val itemPathCompanions = Set[ItemPath.AnyCompanion](
    WorkflowPath)

  //implicit val itemPathJsonCodec: CirceCodec[ItemPath] = ItemPath.jsonCodec(itemPathCompanions)

  //implicit val itemPathJsonDecoder: Decoder[ItemPath] = ItemPath.jsonDecoder(itemPathCompanions.toKeyedMap(_.name).checked)
  object generic {
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
      KeyedSubtype[AgentRefEvent],
      KeyedSubtype[RepoEvent],
      KeyedSubtype[ControllerEvent],
      KeyedSubtype[ClusterEvent],
      KeyedSubtype[AgentRefStateEvent],
      KeyedSubtype[LockEvent],
      KeyedSubtype[OrderEvent])
}
