package js7.agent.data

import js7.agent.data.AgentState.AgentMetaState
import js7.agent.data.event.AgentControllerEvent
import js7.agent.data.event.AgentControllerEvent.AgentCreated
import js7.agent.data.orderwatch.{AllFileWatchesState, FileWatchState}
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, EventId, JournalEvent, JournalState, JournaledState, KeyedEvent, KeyedEventTypedJsonCodec}
import js7.data.item.BasicItemEvent.{ItemAttachedToAgent, ItemDetached}
import js7.data.item.{BasicItemEvent, InventoryItem, InventoryItemEvent}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderForked, OrderJoined, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{FileWatch, OrderWatchEvent, OrderWatchPath}
import js7.data.workflow.{Workflow, WorkflowId}
import monix.reactive.Observable

/**
  * @author Joacim Zschimmer
  */
final case class AgentState(
  eventId: EventId,
  standards: JournaledState.Standards,
  meta: AgentMetaState,
  idToOrder: Map[OrderId, Order[Order.State]],
  idToWorkflow: Map[WorkflowId, Workflow],
  allFileWatchesState: AllFileWatchesState,
  pathToJobResource: Map[JobResourcePath, JobResource])
extends JournaledState[AgentState]
{
  def companion = AgentState

  def estimatedSnapshotSize =
    standards.snapshotSize +
      1 +
      idToWorkflow.size +
      idToOrder.size +
      allFileWatchesState.estimatedSnapshotSize +
      pathToJobResource.size

  def toSnapshotObservable =
    standards.toSnapshotObservable ++
      Observable.fromIterable((meta != AgentMetaState.empty) thenList meta) ++
      Observable.fromIterable(idToWorkflow.values) ++
      Observable.fromIterable(idToOrder.values) ++
      allFileWatchesState.toSnapshot ++
      Observable.fromIterable(pathToJobResource.values)

  def withEventId(eventId: EventId) =
    copy(eventId = eventId)

  def withStandards(standards: JournaledState.Standards) =
    copy(standards = standards)

  def applyEvent(keyedEvent: KeyedEvent[Event]) =
    keyedEvent match {
      case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        applyOrderEvent(orderId, event)

      case KeyedEvent(_, _: AgentControllerEvent.AgentReady) =>
        Right(this)

      case KeyedEvent(orderWatchPath: OrderWatchPath, event: OrderWatchEvent) =>
        allFileWatchesState.applyEvent(orderWatchPath <-: event)
          .map(o => copy(allFileWatchesState = o))

      case KeyedEvent(_: NoKey, event: BasicItemEvent.ForAgent) =>
        event match {
          case ItemAttachedToAgent(workflow: Workflow) =>
            Right(copy(
              idToWorkflow = idToWorkflow + (workflow.id -> workflow)))

          //case ItemDetached(workflowId: WorkflowId, _) =>
          //  Right(copy(
          //    idToWorkflow = idToWorkflow - workflowId))

          case ItemAttachedToAgent(fileWatch: FileWatch) =>
            Right(copy(
              allFileWatchesState = allFileWatchesState.attach(fileWatch)))

          case ItemAttachedToAgent(jobResource: JobResource) =>
            Right(copy(
              pathToJobResource = pathToJobResource + (jobResource.path -> jobResource)))

          case ItemDetached(id: OrderWatchPath, meta.agentPath) =>
            Right(copy(
              allFileWatchesState = allFileWatchesState.detach(id)))

          case _ => applyStandardEvent(keyedEvent)
        }

      case KeyedEvent(_: NoKey, AgentCreated(agentPath, agentRunId, controllerId)) =>
        Right(copy(meta = meta.copy(
          agentPath = agentPath,
          agentRunId = agentRunId,
          controllerId = controllerId)))

      case _ => applyStandardEvent(keyedEvent)
    }

  private def applyOrderEvent(orderId: OrderId, event: OrderEvent) =
    event match {
      case event: OrderEvent.OrderAttachedToAgent =>
        if (idToOrder.contains(orderId))
          Left(Problem.pure(s"Duplicate order attached: $orderId"))
        else
          Right(copy(
            idToOrder = idToOrder + (orderId -> Order.fromOrderAttached(orderId, event))))

      case OrderEvent.OrderDetached =>
        Right(copy(
          idToOrder = idToOrder - orderId))

      case event: OrderCoreEvent =>
        // See also OrderActor#update
        idToOrder.checked(orderId)
          .flatMap(_.applyEvent(event))
          .flatMap(order =>
            event match {
              case event: OrderForked =>
                // TODO Check duplicate child OrderIds
                Right(copy(
                  idToOrder = idToOrder +
                    (order.id -> order) ++
                    idToOrder(orderId).newForkedOrders(event).map(o => o.id -> o)))

              case _: OrderJoined =>
                //order.checkedState[Order.Forked]
                //  .map(order => copy(
                //    idToOrder = idToOrder +
                //      (order.id -> order) --
                //      order.state.childOrderIds))
                Left(Problem.pure("OrderJoined not applicable on AgentState"))

              case _: OrderCoreEvent =>
                Right(copy(
                  idToOrder = idToOrder + (order.id -> order)))
            })

      case _: OrderStdWritten =>
        // OrderStdWritten is not applied (but forwarded to Controller)
        Right(this)
    }

  def isCreated = agentPath.nonEmpty

  def agentPath = meta.agentPath
}

object AgentState extends JournaledState.Companion[AgentState]
{
  val empty = AgentState(EventId.BeforeFirst, JournaledState.Standards.empty,
    AgentMetaState.empty,
    Map.empty, Map.empty, AllFileWatchesState.empty, Map.empty)

  def newBuilder() = new AgentStateBuilder

  protected val InventoryItems: Seq[InventoryItem.Companion_] =
    Seq(FileWatch, Workflow, JobResource)


  final case class AgentMetaState(
    agentPath: AgentPath,
    agentRunId: AgentRunId,
    controllerId: ControllerId)
  object AgentMetaState {
    val empty = AgentMetaState(AgentPath.empty, AgentRunId.empty, ControllerId("NOT-YET-INITIALIZED"))
    implicit val jsonCodec = deriveCodec[AgentMetaState]
  }

  val snapshotObjectJsonCodec: TypedJsonCodec[Any] =
    TypedJsonCodec("AgentState.Snapshot",
      Subtype[JournalState],
      Subtype[AgentMetaState],
      Workflow.subtype,
      Subtype[Order[Order.State]],
      Subtype[FileWatchState.Snapshot],
      Subtype(AgentState.signableSimpleItemJsonCodec))

  implicit val keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    KeyedEventTypedJsonCodec("AgentState.Event",
      KeyedSubtype[JournalEvent],
      KeyedSubtype[OrderEvent],
      KeyedSubtype[AgentControllerEvent],
      KeyedSubtype[InventoryItemEvent],
      KeyedSubtype[OrderWatchEvent])

  object implicits {
    implicit val snapshotObjectJsonCodec = AgentState.snapshotObjectJsonCodec
  }
}
