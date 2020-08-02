package js7.controller.data

import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax._
import js7.controller.data.ControllerSnapshots.ControllerMetaState
import js7.controller.data.agent.{AgentEventIdEvent, AgentSnapshot}
import js7.controller.data.events.ControllerAgentEvent.{AgentCouplingFailed, AgentReady, AgentRegisteredController}
import js7.controller.data.events.ControllerEvent.{ControllerShutDown, ControllerTestEvent}
import js7.controller.data.events.{ControllerAgentEvent, ControllerEvent, ControllerKeyedEventJsonCodec}
import js7.data.agent.AgentRefPath
import js7.data.controller.ControllerFileBaseds
import js7.data.controller.ControllerFileBaseds.ControllerTypedPathCompanions
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{Event, EventId, JournaledState, KeyedEvent}
import js7.data.filebased.{Repo, RepoEvent}
import js7.data.order.OrderEvent.{OrderAdded, OrderCancelled, OrderCoreEvent, OrderFinished, OrderForked, OrderJoined, OrderOffered, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import monix.eval.Task
import monix.reactive.Observable

/**
  * @author Joacim Zschimmer
  */
final case class ControllerState(
  eventId: EventId,
  standards: JournaledState.Standards,
  controllerMetaState: ControllerMetaState,
  repo: Repo,
  pathToAgentSnapshot: Map[AgentRefPath, AgentSnapshot],
  idToOrder: Map[OrderId, Order[Order.State]])
extends JournaledState[ControllerState]
{
  def toSnapshotObservable: Observable[Any] =
    Observable.pure(SnapshotEventId(eventId)) ++
    standards.toSnapshotObservable ++
    Observable.fromIterable(controllerMetaState.isDefined ? controllerMetaState) ++
    Observable.fromIterable(repo.eventsFor(ControllerTypedPathCompanions)) ++
    Observable.fromIterable(pathToAgentSnapshot.values) ++
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

    case KeyedEvent(agentRefPath: AgentRefPath, event: ControllerAgentEvent) =>
      event match {
        case AgentRegisteredController(agentRunId) =>
          pathToAgentSnapshot.checkNoDuplicate(agentRefPath).map(_ =>
            copy(
              pathToAgentSnapshot = pathToAgentSnapshot +
                (agentRefPath -> AgentSnapshot(agentRefPath, Some(agentRunId), eventId = EventId.BeforeFirst))))

        case _: AgentReady | _: AgentCouplingFailed =>
          Right(this)
      }

    case KeyedEvent(a: AgentRefPath, AgentEventIdEvent(agentEventId)) =>
      // Preceding AgentSnapshot is required (see recoverSnapshot)
      for (o <- pathToAgentSnapshot.checked(a)) yield
        copy(pathToAgentSnapshot = pathToAgentSnapshot + (a -> o.copy(eventId = agentEventId)))

    case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
      event match {
        case event: OrderAdded =>
          idToOrder.checkNoDuplicate(orderId).map(_ =>
            copy(idToOrder = idToOrder + (orderId -> Order.fromOrderAdded(orderId, event))))

        case OrderFinished | OrderCancelled =>
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

object ControllerState
{
  val Undefined = ControllerState(
    EventId.BeforeFirst,
    JournaledState.Standards.empty,
    ControllerMetaState.Undefined,
    Repo.ofJsonDecoder(ControllerFileBaseds.jsonCodec),
    Map.empty,
    Map.empty)

  implicit val journaledStateCompanion: JournaledState.Companion[ControllerState] =
    new JournaledState.Companion[ControllerState] {
      val name = "ControllerState"
      val empty = Undefined
      implicit val snapshotObjectJsonCodec = ControllerSnapshots.SnapshotJsonCodec
      implicit val keyedEventJsonDecoder = ControllerKeyedEventJsonCodec

      def fromObservable(snapshotObjects: Observable[Any]) =
        ControllerState.fromObservable(snapshotObjects)
    }

  def fromIterator(snapshotObjects: Iterator[Any]): ControllerState = {
    val builder = new ControllerStateBuilder
    snapshotObjects foreach builder.addSnapshot
    builder.state
  }

  def fromObservable(snapshotObjects: Observable[Any]): Task[ControllerState] =
    Task.defer {
      val builder = new ControllerStateBuilder
      snapshotObjects.foreachL(builder.addSnapshot)
        .map(_ => builder.state)
    }
}
