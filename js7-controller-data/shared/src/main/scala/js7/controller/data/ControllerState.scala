package js7.controller.data

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.base.problem.Problems.DuplicateKey
import js7.base.utils.ScalaUtils.syntax._
import js7.controller.data.agent.AgentRefState
import js7.controller.data.events.AgentRefStateEvent.AgentRegisteredController
import js7.controller.data.events.ControllerEvent.{ControllerShutDown, ControllerTestEvent}
import js7.controller.data.events.{AgentRefStateEvent, ControllerEvent}
import js7.data.agent.AgentRefEvent.{AgentAdded, AgentUpdated}
import js7.data.agent.{AgentName, AgentRef, AgentRefEvent}
import js7.data.cluster.{ClusterEvent, ClusterStateSnapshot}
import js7.data.controller.ControllerItems.ControllerItemPathCompanions
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{Event, EventId, JournalEvent, JournalHeader, JournalState, JournaledState, KeyedEvent, KeyedEventTypedJsonCodec, SnapshotMeta}
import js7.data.item.{Repo, RepoEvent}
import js7.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderForked, OrderJoined, OrderOffered, OrderRemoved, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import monix.reactive.Observable

/**
  * @author Joacim Zschimmer
  */
final case class ControllerState(
  eventId: EventId,
  standards: JournaledState.Standards,
  controllerMetaState: ControllerMetaState,
  nameToAgent: Map[AgentName, AgentRefState],
  repo: Repo,
  idToOrder: Map[OrderId, Order[Order.State]])
extends JournaledState[ControllerState]
{
  def estimatedSnapshotSize: Int =
    2 +
    standards.snapshotSize +
    repo.estimatedEventCount +
    nameToAgent.size +
    idToOrder.size

  def toSnapshotObservable: Observable[Any] =
    Observable.pure(SnapshotEventId(eventId)) ++
    standards.toSnapshotObservable ++
    Observable.fromIterable(controllerMetaState.isDefined ? controllerMetaState) ++
    Observable.fromIterable(repo.eventsFor(ControllerItemPathCompanions)) ++
    Observable.fromIterable(nameToAgent.values) ++
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

    case KeyedEvent(name: AgentName, event: AgentRefEvent) =>
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

    case KeyedEvent(name: AgentName, event: AgentRefStateEvent) =>
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
            childOrdersAddedOrRemoved <- event match {
              case event: OrderForked =>
                Right(idToOrder ++ (
                  for (childOrder <- previousOrder.newForkedOrders(event)) yield
                    childOrder.id -> childOrder))

              case event: OrderJoined =>
                previousOrder.state match {
                  case forked: Order.Forked =>
                    Right(idToOrder -- forked.childOrderIds)

                  case awaiting: Order.Awaiting =>
                    // Offered order is being kept ???
                    //Right(idToOrder - awaiting.offeredOrderId)
                    Right(idToOrder)

                  case state =>
                    Left(Problem(s"For event $event, $orderId must be in state Forked or Awaiting, not: $state"))
                }

              case event: OrderOffered =>
                val offered = previousOrder.newOfferedOrder(event)
                for (_ <- idToOrder.checkNoDuplicate(offered.id)) yield
                  idToOrder + (offered.id -> offered)

              case _ => Right(idToOrder)
            }
          } yield
            copy(idToOrder = childOrdersAddedOrRemoved + (updatedOrder.id -> updatedOrder))

        case _: OrderStdWritten =>
          Right(this)
      }

    case KeyedEvent(_, _: ControllerShutDown) =>
      Right(this)

    case KeyedEvent(_, ControllerTestEvent) =>
      Right(this)

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
    Repo.empty,
    Map.empty)

  val empty = Undefined

  def newBuilder() = new ControllerStateBuilder

  val snapshotObjectJsonCodec: TypedJsonCodec[Any] = {
    import js7.data.controller.ControllerItems._
    TypedJsonCodec[Any](
      Subtype[JournalHeader],
      Subtype[SnapshotMeta],
      Subtype[JournalState],
      Subtype(deriveCodec[ClusterStateSnapshot]),
      Subtype(deriveCodec[ControllerMetaState]),
      Subtype[AgentRefState],
      Subtype[RepoEvent],  // These events describe complete objects
      Subtype[AgentRegisteredController],  // These events describe complete objects
      Subtype[Order[Order.State]])
  }

  implicit val keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] = {
    import js7.data.controller.ControllerItems._
    KeyedEventTypedJsonCodec[Event](
      KeyedSubtype[JournalEvent],
      KeyedSubtype[AgentRefEvent],
      KeyedSubtype[RepoEvent],
      KeyedSubtype[ControllerEvent],
      KeyedSubtype[ClusterEvent],
      KeyedSubtype[AgentRefStateEvent],
      KeyedSubtype[OrderEvent])
  }
}
