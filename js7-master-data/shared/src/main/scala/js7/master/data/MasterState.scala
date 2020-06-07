package js7.master.data

import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.RichPartialFunction
import js7.base.utils.ScalazStyle._
import js7.data.agent.AgentRefPath
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, EventId, JournaledState, KeyedEvent}
import js7.data.filebased.{Repo, RepoEvent}
import js7.data.master.MasterFileBaseds
import js7.data.master.MasterFileBaseds.MasterTypedPathCompanions
import js7.data.order.OrderEvent.{OrderAdded, OrderCancelled, OrderCoreEvent, OrderFinished, OrderForked, OrderJoined, OrderOffered, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.master.data.MasterSnapshots.MasterMetaState
import js7.master.data.agent.{AgentEventIdEvent, AgentSnapshot}
import js7.master.data.events.MasterAgentEvent.{AgentCouplingFailed, AgentReady, AgentRegisteredMaster}
import js7.master.data.events.MasterEvent.{MasterShutDown, MasterTestEvent}
import js7.master.data.events.{MasterAgentEvent, MasterEvent, MasterKeyedEventJsonCodec}
import monix.reactive.Observable

/**
  * @author Joacim Zschimmer
  */
final case class MasterState(
  eventId: EventId,
  standards: JournaledState.Standards,
  masterMetaState: MasterMetaState,
  repo: Repo,
  pathToAgentSnapshot: Map[AgentRefPath, AgentSnapshot],
  idToOrder: Map[OrderId, Order[Order.State]])
extends JournaledState[MasterState]
{
  def toSnapshotObservable: Observable[Any] =
    standards.toSnapshotObservable ++
    Observable.fromIterable(masterMetaState.isDefined ? masterMetaState) ++
    Observable.fromIterable(repo.eventsFor(MasterTypedPathCompanions)) ++
    Observable.fromIterable(pathToAgentSnapshot.values) ++
    Observable.fromIterable(idToOrder.values)

  def withEventId(eventId: EventId) =
    copy(eventId = eventId)

  def withStandards(standards: JournaledState.Standards) =
    copy(standards = standards)

  def applyEvent(keyedEvent: KeyedEvent[Event]) = keyedEvent match {
    case KeyedEvent(_: NoKey, MasterEvent.MasterInitialized(masterId, startedAt)) =>
      Right(copy(masterMetaState = masterMetaState.copy(
        masterId = masterId,
        startedAt = startedAt)))

    case KeyedEvent(_: NoKey, event: MasterEvent.MasterReady) =>
      Right(copy(masterMetaState = masterMetaState.copy(
        timezone = event.timezone)))

    case KeyedEvent(_: NoKey, _: MasterEvent.MasterShutDown) =>
      Right(this)

    case KeyedEvent(_: NoKey, MasterEvent.MasterTestEvent) =>
      Right(this)

    case KeyedEvent(_: NoKey, event: RepoEvent) =>
      for (o <- repo.applyEvent(event)) yield
        copy(repo = o)

    case KeyedEvent(agentRefPath: AgentRefPath, event: MasterAgentEvent) =>
      event match {
        case AgentRegisteredMaster(agentRunId) =>
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

    case KeyedEvent(_, _: MasterShutDown) =>
      Right(this)

    case KeyedEvent(_, MasterTestEvent) =>
      Right(this)

    case _ => applyStandardEvent(keyedEvent)
  }

  override def toString =
    s"MasterState(${EventId.toString(eventId)} ${idToOrder.size} orders, Repo(${repo.currentVersionSize} objects, ...))"
}

object MasterState
{
  val Undefined = MasterState(
    EventId.BeforeFirst,
    JournaledState.Standards.empty,
    MasterMetaState.Undefined,
    Repo.ofJsonDecoder(MasterFileBaseds.jsonCodec),
    Map.empty,
    Map.empty)

  implicit val journaledStateCompanion: JournaledState.Companion[MasterState] =
    new JournaledState.Companion[MasterState] {
      def fromIterable(snapshotObjects: Iterable[Any]) =
        MasterState.fromIterable(snapshotObjects)

      implicit def snapshotObjectJsonCodec = MasterSnapshots.SnapshotJsonCodec

      implicit val keyedEventJsonDecoder = MasterKeyedEventJsonCodec
    }

  def fromIterable(snapshotObjects: Iterable[Any]): MasterState =
    fromIterator(snapshotObjects.iterator)

  def fromIterator(snapshotObjects: Iterator[Any]): MasterState = {
    val builder = new MasterStateBuilder
    snapshotObjects foreach builder.addSnapshot
    builder.state
  }
}
