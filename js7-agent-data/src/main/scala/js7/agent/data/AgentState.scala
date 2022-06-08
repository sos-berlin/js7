package js7.agent.data

import io.circe.generic.semiauto.deriveCodec
import js7.agent.data.AgentState.{AgentMetaState, allowedItemStates}
import js7.agent.data.event.AgentEvent
import js7.agent.data.event.AgentEvent.AgentDedicated
import js7.agent.data.orderwatch.{FileWatchState, FileWatchStateHandler}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.Signed
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.Tests.isTest
import js7.data.agent.{AgentPath, AgentRefState, AgentRunId}
import js7.data.calendar.{Calendar, CalendarPath, CalendarState}
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, EventId, ItemContainer, JournalEvent, JournalState, KeyedEvent, KeyedEventTypedJsonCodec, SignedItemContainer, SnapshotableState}
import js7.data.item.BasicItemEvent.{ItemAttachedToMe, ItemDetached, ItemDetachingFromMe, SignedItemAttachedToMe}
import js7.data.item.SignedItemEvent.SignedItemAdded
import js7.data.item.{BasicItemEvent, InventoryItem, InventoryItemEvent, InventoryItemKey, InventoryItemState, SignableItem, SignableItemKey, UnsignedSimpleItemPath, UnsignedSimpleItemState}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{FileWatch, OrderWatchEvent, OrderWatchPath}
import js7.data.state.EventDrivenStateView
import js7.data.subagent.SubagentItemStateEvent.SubagentShutdown
import js7.data.subagent.{SubagentDirectorState, SubagentId, SubagentItem, SubagentItemState, SubagentItemStateEvent, SubagentSelection, SubagentSelectionId, SubagentSelectionState}
import js7.data.workflow.{Workflow, WorkflowControlEvent, WorkflowControlState, WorkflowControlStateHandler, WorkflowId, WorkflowPath}
import monix.reactive.Observable
import scala.collection.MapView

/**
  * @author Joacim Zschimmer
  */
final case class AgentState(
  eventId: EventId,
  standards: SnapshotableState.Standards,
  meta: AgentMetaState,
  pathToItemState_ : Map[UnsignedSimpleItemPath, UnsignedSimpleItemState],
  idToOrder: Map[OrderId, Order[Order.State]],
  idToWorkflow: Map[WorkflowId, Workflow/*reduced for this Agent!!!*/],
  pathToWorkflowControlState_ : Map[WorkflowPath, WorkflowControlState],
  pathToJobResource: Map[JobResourcePath, JobResource],
  keyToSignedItem : Map[SignableItemKey, Signed[SignableItem]])
extends SignedItemContainer
with EventDrivenStateView[AgentState, Event]
with SubagentDirectorState[AgentState]
with FileWatchStateHandler[AgentState]
with WorkflowControlStateHandler[AgentState]
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
      idToWorkflow.size +
      idToOrder.size +
      pathToItemState_.size +
      pathToWorkflowControlState_.size +
      fw.estimatedExtraSnapshotSize +
      pathToJobResource.size
      //keyToSignedItem.size +  // == idToWorkflow.size + pathToJobResource.size

  def toSnapshotObservable = Observable(
    standards.toSnapshotObservable,
    Observable.fromIterable(meta != AgentMetaState.empty thenList meta),
    Observable.fromIterable(pathTo(SubagentItemState).values).flatMap(_.toSnapshotObservable),
    Observable.fromIterable(pathTo(SubagentSelectionState).values).flatMap(_.toSnapshotObservable),
    Observable.fromIterable(pathTo(FileWatchState).values).flatMap(_.toSnapshotObservable),
    Observable.fromIterable(keyToSignedItem.values.view.map(SignedItemAdded(_))),
    Observable.fromIterable(idToWorkflow.view.filterKeys(isWithoutSignature).values),
    Observable.fromIterable(pathToWorkflowControlState_.values),
    Observable.fromIterable(pathToJobResource.view.filterKeys(isWithoutSignature).values),
    Observable.fromIterable(pathTo(CalendarState).values).flatMap(_.toSnapshotObservable),
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
        fw.applyEvent(orderWatchPath <-: event)

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
            // COMPATIBLE with v2.2.0. Since v2.2.2, workflow is transferred via SignedItemAttachedToMe
            for (o <- idToWorkflow.insert(workflow.id -> workflow.reduceForAgent(agentPath))) yield
              copy(
                idToWorkflow = o)

          case ItemAttachedToMe(fileWatch: FileWatch) =>
            // May replace an existing JobResource
            fw.attach(fileWatch)

          case ItemAttachedToMe(jobResource: JobResource) =>
            // COMPATIBLE with v2.2.0. Since v2.2.2, workflow is transferred via SignedItemAttachedToMe
             // May replace an existing JobResource
            Right(copy(
              pathToJobResource = pathToJobResource + (jobResource.path -> jobResource)))

          case ItemAttachedToMe(calendar: Calendar) =>
            // May replace an existing Calendar
            Right(copy(
              pathToItemState_ = pathToItemState_.updated(calendar.path, CalendarState(calendar))))

          case ItemAttachedToMe(subagentItem: SubagentItem) =>
            // May replace an existing SubagentItem
            Right(copy(
              pathToItemState_ = pathToItemState_.updated(subagentItem.id,
                pathTo(SubagentItemState)
                  .get(subagentItem.id)
                  .match_ {
                    case None => SubagentItemState.initial(subagentItem)
                    case Some(subagentState) => subagentState.copy(subagentItem = subagentItem)
                  })))

          case ItemAttachedToMe(selection: SubagentSelection) =>
            Right(copy(
              pathToItemState_ = pathToItemState_.updated(selection.id, SubagentSelectionState(selection))))

          case ItemDetached(itemKey, meta.agentPath) =>
            itemKey match {
              case WorkflowId.as(workflowId) =>
                for (_ <- idToWorkflow.checked(workflowId)) yield {
                  val updatedIdToWorkflow = idToWorkflow - workflowId
                  copy(
                    keyToSignedItem = keyToSignedItem - workflowId,
                    idToWorkflow = updatedIdToWorkflow,
                    pathToWorkflowControlState_ =
                      if (idToWorkflow.keys.exists/*Slow???*/(_.path == workflowId.path))
                        pathToWorkflowControlState_ - workflowId.path
                      else
                       pathToWorkflowControlState_)
                }

              case path: OrderWatchPath =>
                fw.detach(path)

              case path: JobResourcePath =>
                for (_ <- pathToJobResource.checked(path)) yield
                  copy(
                    keyToSignedItem = keyToSignedItem - path,
                    pathToJobResource = pathToJobResource - path)

              case path: CalendarPath =>
                for (_ <- pathToItemState_.checked(path)) yield
                  copy(
                    pathToItemState_ = pathToItemState_ - path)

              case id: SubagentId =>
                for (_ <- pathToItemState_.checked(id)) yield
                  copy(
                    pathToItemState_ = pathToItemState_ - id)

              case id: SubagentSelectionId =>
                Right(copy(
                  pathToItemState_ = pathToItemState_ - id))

              case _ => applyStandardEvent(keyedEvent)
            }

          case ItemDetachingFromMe(id: SubagentId) =>
            for (subagentItemState <- pathTo(SubagentItemState).checked(id)) yield
              copy(
                pathToItemState_ = pathToItemState_.updated(id,
                  subagentItemState.copy(isDetaching = true)))

          case _ => applyStandardEvent(keyedEvent)
        }

      case KeyedEvent(subagentId: SubagentId, event: SubagentItemStateEvent) =>
        event match {
          case SubagentShutdown if !pathToItemState_.contains(subagentId) =>
            // May arrive when SubagentItem has been deleted
            Right(this)

          case _ =>
            for {
              subagentItemState <- pathTo(SubagentItemState).checked(subagentId)
              subagentItemState <- subagentItemState.applyEvent(event)
            } yield copy(
              pathToItemState_ = pathToItemState_.updated(subagentId, subagentItemState))
        }

      case KeyedEvent(_: NoKey, AgentDedicated(subagentId, agentPath, agentRunId, controllerId)) =>
        Right(copy(meta = meta.copy(
          agentPath = agentPath,
          agentRunId = agentRunId,
          controllerId = controllerId,
          subagentId = subagentId)))

      case KeyedEvent(workflowPath: WorkflowPath, event: WorkflowControlEvent) =>
        applyWorkflowControlEvent(workflowPath, event)

      case _ => applyStandardEvent(keyedEvent)
    }

  def pathToItemState = pathToItemState_.view

  def idToSubagentItemState = pathTo(SubagentItemState)

  protected def pathToFileWatchState = pathTo(FileWatchState)

  protected def updateFileWatchStates(
    fileWatchStates: Iterable[FileWatchState],
    remove: Iterable[OrderWatchPath]
  ) = update(addItemStates = fileWatchStates, removeItemStates = remove)

  def pathToWorkflowControlState = pathToWorkflowControlState_.view

  protected def updateWorkflowControlState(s: WorkflowControlState) =
    copy(pathToWorkflowControlState_ = pathToWorkflowControlState_.updated(s.workflowPath, s))

  protected def update(
    orders: Iterable[Order[Order.State]],
    removeOrders: Iterable[OrderId],
    addItemStates: Iterable[UnsignedSimpleItemState],
    removeItemStates: Iterable[UnsignedSimpleItemPath])
  : Checked[AgentState] =
    if (isTest && !addItemStates.forall(o => allowedItemStates(o.companion)))
      Left(Problem.pure("Unsupported InventoryItemState"))
    else
      Right(copy(
        idToOrder = idToOrder -- removeOrders ++ orders.map(o => o.id -> o),
        pathToItemState_ = pathToItemState_
          -- removeItemStates ++ addItemStates.map(o => o.path -> o)))

  def agentPath = meta.agentPath

  lazy val keyToItem: MapView[InventoryItemKey, InventoryItem] =
    new MapView[InventoryItemKey, InventoryItem] {
      def get(itemKey: InventoryItemKey): Option[InventoryItem] =
        itemKey match {
          case path: JobResourcePath => pathToJobResource.get(path)
          case WorkflowId.as(id) => idToWorkflow.get(id)
          case path: UnsignedSimpleItemPath => pathToItemState_.get(path).map(_.item)
        }

      def iterator: Iterator[(InventoryItemKey, InventoryItem)] =
        pathToJobResource.iterator ++
          idToWorkflow.iterator ++
          pathToItemState.mapValues(_.item).iterator
    }

  def keyToSigned[I <: SignableItem](I: SignableItem.Companion[I]): MapView[I.Key, Signed[I]] =
    new MapView[I.Key, Signed[I]] {
      def get(key: I.Key) =
        keyToSignedItem.get(key).asInstanceOf[Option[Signed[I]]]

      def iterator =
        keyToSignedItem.iterator.collect {
          case pair @ (_, Signed(item, _)) if item.companion eq I =>
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

  def orders = idToOrder.values
}

object AgentState
extends SnapshotableState.Companion[AgentState]
with ItemContainer.Companion[AgentState]
{
  type StateEvent = Event

  val empty = AgentState(EventId.BeforeFirst, SnapshotableState.Standards.empty,
    AgentMetaState.empty,
    Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  private val allowedItemStates: Set[InventoryItemState.AnyCompanion] =
    Set(AgentRefState, SubagentItemState, FileWatchState)

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

  val snapshotObjectJsonCodec = TypedJsonCodec[Any](
    Subtype[JournalState],
    Subtype[AgentMetaState],
    Workflow.subtype,
    Subtype[SubagentItemState](aliases = Seq("SubagentRefState")),
    Subtype[Order[Order.State]],
    Subtype[FileWatchState.Snapshot],
    Subtype(SignedItemAdded.jsonCodec(this)),  // For Repo and SignedItemAdded
    Subtype(signableSimpleItemJsonCodec),
    Subtype(unsignedSimpleItemJsonCodec),
    Subtype[BasicItemEvent],
    Subtype[WorkflowControlState])

  implicit val keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] = {
    KeyedEventTypedJsonCodec[Event](
      KeyedSubtype[JournalEvent],
      KeyedSubtype[SubagentItemStateEvent],
      KeyedSubtype[OrderEvent],
      KeyedSubtype[AgentEvent],
      KeyedSubtype[InventoryItemEvent],
      KeyedSubtype[OrderWatchEvent],
      KeyedSubtype[WorkflowControlEvent])
  }
}
