package js7.agent.data

import js7.agent.data.AgentState.AgentMetaState
import js7.agent.data.event.AgentEvent
import js7.agent.data.event.AgentEvent.AgentDedicated
import js7.agent.data.orderwatch.{AllFileWatchesState, FileWatchState}
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.Signed
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.calendar.{Calendar, CalendarPath}
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, EventId, ItemContainer, JournalEvent, JournalState, KeyedEvent, KeyedEventTypedJsonCodec, SignedItemContainer, SnapshotableState}
import js7.data.item.BasicItemEvent.{ItemAttachedToMe, ItemDetached, ItemDetachingFromMe, SignedItemAttachedToMe}
import js7.data.item.SignedItemEvent.SignedItemAdded
import js7.data.item.{BasicItemEvent, InventoryItem, InventoryItemEvent, InventoryItemKey, SignableItem, SignableItemKey}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderForked, OrderJoined, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{FileWatch, OrderWatchEvent, OrderWatchPath}
import js7.data.state.{AgentStateView, StateView}
import js7.data.subagent.SubagentItemStateEvent.SubagentShutdown
import js7.data.subagent.{SubagentId, SubagentItem, SubagentItemState, SubagentItemStateEvent, SubagentSelection, SubagentSelectionId}
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import monix.reactive.Observable
import scala.collection.MapView

/**
  * @author Joacim Zschimmer
  */
final case class AgentState(
  eventId: EventId,
  standards: SnapshotableState.Standards,
  meta: AgentMetaState,
  idToSubagentItemState: Map[SubagentId, SubagentItemState],
  idToSubagentSelection: Map[SubagentSelectionId, SubagentSelection],
  idToOrder: Map[OrderId, Order[Order.State]],
  idToWorkflow: Map[WorkflowId, Workflow/*reduced for this Agent!!!*/],
  allFileWatchesState: AllFileWatchesState,
  pathToJobResource: Map[JobResourcePath, JobResource],
  pathToCalendar: Map[CalendarPath, Calendar],
  keyToSignedItem : Map[SignableItemKey, Signed[SignableItem]])
extends SignedItemContainer
with StateView
with AgentStateView
with SnapshotableState[AgentState]
{
  def isAgent = true

  def controllerId = meta.controllerId

  def companion = AgentState

  /** A Controller has initialized this Agent? */
  def isDedicated =
    agentPath.nonEmpty/*shortcut*/ ||
      copy(eventId = EventId.BeforeFirst) != AgentState.empty

  def isFreshlyDedicated: Boolean =
    isDedicated &&
      this == AgentState.empty.copy(
        eventId = eventId,
        standards = standards,
        meta = meta)

  def estimatedSnapshotSize =
    standards.snapshotSize +
      1 +
      idToSubagentItemState.size +
      idToSubagentSelection.size +
      idToWorkflow.size +
      idToOrder.size +
      allFileWatchesState.estimatedSnapshotSize +
      pathToJobResource.size +
      //keyToSignedItem.size +  // == idToWorkflow.size + pathToJobResource.size
      pathToCalendar.size

  def toSnapshotObservable = Observable(
    standards.toSnapshotObservable,
    Observable.fromIterable(meta != AgentMetaState.empty thenList meta),
    Observable.fromIterable(idToSubagentItemState.values),
    Observable.fromIterable(idToSubagentSelection.values),
    allFileWatchesState.toSnapshot,
    Observable.fromIterable(keyToSignedItem.values.view.map(SignedItemAdded(_))),
    Observable.fromIterable(idToWorkflow.view.filterKeys(isWithoutSignature).values),
    Observable.fromIterable(pathToJobResource.view.filterKeys(isWithoutSignature).values),
    Observable.fromIterable(pathToCalendar.values),
    Observable.fromIterable(idToOrder.values)
  ).flatten

  // COMPATIBLE with v2.2
  private def isWithoutSignature(itemKey: SignableItemKey) =
    !keyToSignedItem.contains(itemKey)

  def withEventId(eventId: EventId) =
    copy(eventId = eventId)

  def withStandards(standards: SnapshotableState.Standards) =
    copy(standards = standards)

  def applyEvent(keyedEvent: KeyedEvent[Event]): Checked[AgentState] =
    keyedEvent match {
      case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        applyOrderEvent(orderId, event)

      case KeyedEvent(_, _: AgentEvent.AgentReady) =>
        Right(this)

      case KeyedEvent(_, AgentEvent.AgentShutDown) =>
        Right(this)

      case KeyedEvent(orderWatchPath: OrderWatchPath, event: OrderWatchEvent) =>
        allFileWatchesState.applyEvent(orderWatchPath <-: event)
          .map(o => copy(allFileWatchesState = o))

      case KeyedEvent(_: NoKey, event: BasicItemEvent.ForDelegate) =>
        event match {
          case SignedItemAttachedToMe(signed: Signed[SignableItem]) =>
            Right(signed.value match {
              case workflow: Workflow =>
                copy(
                  keyToSignedItem = keyToSignedItem + (workflow.id -> signed),
                  idToWorkflow = idToWorkflow + (workflow.id -> workflow.reduceForAgent(agentPath)))

              case jobResource: JobResource  =>
                copy(
                  keyToSignedItem = keyToSignedItem + (jobResource.path -> signed),
                  pathToJobResource = pathToJobResource + (jobResource.path -> jobResource))
            })

          case ItemAttachedToMe(workflow: Workflow) =>
            // COMPATIBLE with v2.2
            for (o <- idToWorkflow.insert(workflow.id -> workflow.reduceForAgent(agentPath))) yield
              copy(
                idToWorkflow = o)

          case ItemAttachedToMe(fileWatch: FileWatch) =>
            // May replace an existing JobResource
            Right(copy(
              allFileWatchesState = allFileWatchesState.attach(fileWatch)))

          case ItemAttachedToMe(jobResource: JobResource) =>
            // COMPATIBLE with v2.2
            // May replace an existing JobResource
            Right(copy(
              pathToJobResource = pathToJobResource + (jobResource.path -> jobResource)))

          case ItemAttachedToMe(calendar: Calendar) =>
            // May replace an existing Calendar
            Right(copy(
              pathToCalendar = pathToCalendar + (calendar.path -> calendar)))

          case ItemAttachedToMe(subagentItem: SubagentItem) =>
            // May replace an existing SubagentItem
            Right(copy(
              idToSubagentItemState = idToSubagentItemState + (subagentItem.id ->
                idToSubagentItemState
                  .get(subagentItem.id)
                  .match_ {
                    case None => SubagentItemState.initial(subagentItem)
                    case Some(subagentState) => subagentState.copy(subagentItem = subagentItem)
                  })))

          case ItemAttachedToMe(selection: SubagentSelection) =>
            Right(copy(
              idToSubagentSelection = idToSubagentSelection.updated(selection.id, selection)))

          case ItemDetached(itemKey, meta.agentPath) =>
            itemKey match {
              case WorkflowId.as(workflowId) =>
                for (_ <- idToWorkflow.checked(workflowId)) yield
                  copy(
                    keyToSignedItem = keyToSignedItem - workflowId,
                    idToWorkflow = idToWorkflow - workflowId)

              case path: OrderWatchPath =>
                for (_ <- allFileWatchesState.pathToFileWatchState.checked(path)) yield
                  copy(
                    allFileWatchesState = allFileWatchesState.detach(path))

              case path: JobResourcePath =>
                for (_ <- pathToJobResource.checked(path)) yield
                  copy(
                    keyToSignedItem = keyToSignedItem - path,
                    pathToJobResource = pathToJobResource - path)

              case path: CalendarPath =>
                for (_ <- pathToCalendar.checked(path)) yield
                  copy(
                    pathToCalendar = pathToCalendar - path)

              case id: SubagentId =>
                for (_ <- idToSubagentItemState.checked(id)) yield
                  copy(
                    idToSubagentItemState = idToSubagentItemState - id)

              case id: SubagentSelectionId =>
                Right(copy(
                  idToSubagentSelection = idToSubagentSelection - id))

              case _ => applyStandardEvent(keyedEvent)
            }

          case ItemDetachingFromMe(id: SubagentId) =>
            for (subagentItemState <- idToSubagentItemState.checked(id)) yield
              copy(
                idToSubagentItemState = idToSubagentItemState.updated(id,
                  subagentItemState.copy(isDetaching = true)))

          case _ => applyStandardEvent(keyedEvent)
        }

      case KeyedEvent(subagentId: SubagentId, event: SubagentItemStateEvent) =>
        event match {
          case SubagentShutdown if !idToSubagentItemState.contains(subagentId) =>
            // May arrive when SubagentItem has been deleted
            Right(this)

          case _ =>
            for {
              subagentItemState <- idToSubagentItemState.checked(subagentId)
              subagentItemState <- subagentItemState.applyEvent(event)
            } yield copy(
              idToSubagentItemState = idToSubagentItemState + (subagentId -> subagentItemState))
        }

      case KeyedEvent(_: NoKey, AgentDedicated(subagentId, agentPath, agentRunId, controllerId)) =>
        Right(copy(meta = meta.copy(
          agentPath = agentPath,
          agentRunId = agentRunId,
          controllerId = controllerId,
          subagentId = subagentId)))

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
        // But check OrderId
        idToOrder.checked(orderId).rightAs(this)
    }

  def agentPath = meta.agentPath

  lazy val keyToItem: MapView[InventoryItemKey, InventoryItem] =
    new MapView[InventoryItemKey, InventoryItem] {
      def get(itemKey: InventoryItemKey): Option[InventoryItem] =
        itemKey match {
          case path: JobResourcePath => pathToJobResource.get(path)
          case path: CalendarPath => pathToCalendar.get(path)
          case path: OrderWatchPath => allFileWatchesState.pathToFileWatchState.get(path).map(_.fileWatch)
          case WorkflowId.as(id) => idToWorkflow.get(id)
          case id: SubagentId => idToSubagentItemState.get(id).map(_.subagentItem)
          case id: SubagentSelectionId => idToSubagentSelection.get(id)
        }

      def iterator: Iterator[(InventoryItemKey, InventoryItem)] =
        pathToJobResource.iterator ++
          pathToCalendar.iterator ++
          allFileWatchesState.pathToFileWatchState.view.mapValues(_.fileWatch).iterator ++
          idToWorkflow.iterator ++
          idToSubagentItemState.view.mapValues(_.item).iterator ++
          idToSubagentSelection.iterator
    }

  def keyToSigned[I <: SignableItem](I: SignableItem.Companion[I]): MapView[I.Key, Signed[I]] =
    new MapView[I.Key, Signed[I]] {
      def get(key: I.Key) =
        keyToSignedItem.get(key).asInstanceOf[Option[Signed[I]]]

      def iterator =
        keyToSignedItem.iterator.collect {
          case pair @ (_, Signed(item, _)) if I.cls.isAssignableFrom(item.getClass) =>
            pair.asInstanceOf[(I.Key, Signed[I])]
        }

      //? override def values =
      //  keyToSignedItem.values.view.collect {
      //    case signed @ Signed(item: I, _) if I.cls.isAssignableFrom(item.getClass) =>
      //      signed.asInstanceOf[Signed[I]]
      //  }
    }

  def workflowPathToId(workflowPath: WorkflowPath) =
    Left(Problem.pure("workflowPathToId is not available at Agent"))

  def pathToLockState = Map.empty
  def pathToBoardState = Map.empty

  def orders = idToOrder.values
}

object AgentState
extends SnapshotableState.Companion[AgentState]
with ItemContainer.Companion[AgentState]
{
  type StateEvent = Event

  val empty = AgentState(EventId.BeforeFirst, SnapshotableState.Standards.empty,
    AgentMetaState.empty,
    Map.empty, Map.empty, Map.empty, Map.empty, AllFileWatchesState.empty,
    Map.empty, Map.empty, Map.empty)

  def newBuilder() = new AgentStateBuilder

  protected val inventoryItems = Vector(
    FileWatch, JobResource, Calendar, Workflow, SubagentItem, SubagentSelection)

  override lazy val itemPaths =
    inventoryItems.map(_.Path) :+ AgentPath

  final case class AgentMetaState(
    subagentId: Option[SubagentId],
    agentPath: AgentPath,
    agentRunId: AgentRunId,
    controllerId: ControllerId)
  object AgentMetaState
  {
    val empty = AgentMetaState(
      None,
      AgentPath.empty,
      AgentRunId.empty,
      ControllerId("NOT-YET-INITIALIZED"))

    implicit val jsonCodec = deriveCodec[AgentMetaState]
  }

  val snapshotObjectJsonCodec: TypedJsonCodec[Any] =
    TypedJsonCodec.named("AgentState.snapshotObjectJsonCodec",
      Subtype[JournalState],
      Subtype[AgentMetaState],
      Workflow.subtype,
      Subtype[SubagentItemState],
      Subtype[Order[Order.State]],
      Subtype[FileWatchState.Snapshot],
      Subtype(SignedItemAdded.jsonCodec(this)),  // For Repo and SignedItemAdded
      Subtype(signableSimpleItemJsonCodec),  // COMPATIBLE with v2.2
      Subtype(unsignedSimpleItemJsonCodec),
      Subtype[BasicItemEvent])

  implicit val keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    KeyedEventTypedJsonCodec.named("AgentState.Event",
      KeyedSubtype[JournalEvent],
      KeyedSubtype[SubagentItemStateEvent],
      KeyedSubtype[OrderEvent],
      KeyedSubtype[AgentEvent],
      KeyedSubtype[InventoryItemEvent],
      KeyedSubtype[OrderWatchEvent])
}
