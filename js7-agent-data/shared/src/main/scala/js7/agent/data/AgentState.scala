package js7.agent.data

import fs2.Stream
import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Decoder, Encoder}
import js7.agent.data.AgentState.{AgentMetaState, allowedItemStates}
import js7.agent.data.event.AgentEvent
import js7.agent.data.event.AgentEvent.AgentDedicated
import js7.agent.data.orderwatch.{FileWatchState, FileWatchStateRecoverer}
import js7.base.auth.UserId
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.Signed
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isStrict
import js7.data.agent.{AgentPath, AgentRef, AgentRefState, AgentRunId}
import js7.data.calendar.{Calendar, CalendarPath, CalendarState}
import js7.data.cluster.{ClusterEvent, ClusterStateSnapshot}
import js7.data.controller.{ControllerId, ControllerRunId}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{ClusterableState, Event, EventId, ItemContainer, JournalEvent, JournalState, KeyedEvent, KeyedEventTypedJsonCodec, SignedItemContainer, SnapshotableState}
import js7.data.item.BasicItemEvent.{ItemAttachedToMe, ItemDetached, ItemDetachingFromMe, SignedItemAttachedToMe}
import js7.data.item.SignedItemEvent.SignedItemAdded
import js7.data.item.{BasicItemEvent, InventoryItem, InventoryItemEvent, InventoryItemKey, InventoryItemState, SignableItem, SignableItemKey, UnsignedItem, UnsignedItemKey, UnsignedItemState, UnsignedSimpleItemPath, UnsignedSimpleItemState}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.node.{NodeId, NodeName}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{FileWatch, OrderWatchEvent, OrderWatchPath}
import js7.data.state.EventDrivenStateView
import js7.data.subagent.SubagentItemStateEvent.SubagentShutdown
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentBundleState, SubagentDirectorState, SubagentId, SubagentItem, SubagentItemState, SubagentItemStateEvent}
import js7.data.system.ServerMeteringEvent
import js7.data.workflow.{Workflow, WorkflowControl, WorkflowControlId, WorkflowId, WorkflowPath, WorkflowPathControl, WorkflowPathControlPath}
import scala.collection.MapView

/**
  * @author Joacim Zschimmer
  */
final case class AgentState(
  eventId: EventId,
  standards: SnapshotableState.Standards,
  meta: AgentMetaState,
  keyToUnsignedItemState_ : Map[UnsignedItemKey, UnsignedItemState],
  idToOrder: Map[OrderId, Order[Order.State]],
  idToWorkflow: Map[WorkflowId, Workflow/*reduced for this Agent!!!*/],
  pathToJobResource: Map[JobResourcePath, JobResource],
  keyToSignedItem : Map[SignableItemKey, Signed[SignableItem]])
extends SignedItemContainer,
  EventDrivenStateView[AgentState],
  SubagentDirectorState[AgentState],
  FileWatchStateRecoverer[AgentState],
  ClusterableState[AgentState]:

  override def isAgent = true

  override def maybeAgentPath: Option[AgentPath] =
    Some(meta.agentPath)

  def agentRunId: AgentRunId =
    meta.agentRunId

  def controllerId: ControllerId =
    meta.controllerId

  def companion: AgentState.type = AgentState

  def name: String =
    if meta.agentPath.isEmpty then
      "Agent not dedicated"
    else
      meta.agentPath.toString

  /** A Controller has initialized this Agent? */
  def isDedicated: Boolean =
    agentPath.nonEmpty/*shortcut*/

  def isFreshlyDedicated: Boolean =
    isDedicated &&
      this == AgentState.empty.copy(
        eventId = eventId,
        standards = standards,
        meta = meta)

  def estimatedSnapshotSize: Int =
    standards.snapshotSize +
      1 +
      idToWorkflow.size +
      idToOrder.size +
      keyToUnsignedItemState_.size +
      fw.estimatedExtraSnapshotSize +
      pathToJobResource.size
      //keyToSignedItem.size +  // == idToWorkflow.size + pathToJobResource.size

  def toSnapshotStream: Stream[fs2.Pure, Any] = Stream(
    standards.toSnapshotStream,
    Stream.iterable(meta != AgentMetaState.empty thenList meta),
    Stream.iterable(keyToItem(AgentRef).values),
    Stream.iterable(keyTo(SubagentItemState).values).flatMap(_.toSnapshotStream),
    Stream.iterable(keyTo(SubagentBundleState).values).flatMap(_.toSnapshotStream),
    Stream.iterable(keyTo(FileWatchState).values).flatMap(_.toSnapshotStream),
    Stream.iterable(keyToSignedItem.values.view.map(SignedItemAdded(_))),
    Stream.iterable(idToWorkflow.view.filterKeys(isWithoutSignature).values),
    Stream.iterable(pathToJobResource.view.filterKeys(isWithoutSignature).values),
    Stream.iterable(keyTo(CalendarState).values).flatMap(_.toSnapshotStream),
    Stream.iterable(keyTo(WorkflowPathControl).values).flatMap(_.toSnapshotStream),
    Stream.iterable(keyTo(WorkflowControl).values).flatMap(_.toSnapshotStream),
    Stream.iterable(idToOrder.values)
  ).flatten

  // COMPATIBLE with v2.2
  private def isWithoutSignature(itemKey: SignableItemKey) =
    !keyToSignedItem.contains(itemKey)

  def withEventId(eventId: EventId): AgentState =
    copy(eventId = eventId)

  def withStandards(standards: SnapshotableState.Standards): AgentState =
    copy(standards = standards)

  override protected def applyOrderEvent(orderId: OrderId, event: OrderEvent): Checked[AgentState] =
    event match
      case event: OrderEvent.OrderAttachedToAgent =>
        if idToOrder isDefinedAt orderId then
          Left(Problem.pure(s"Duplicate order attached: $orderId"))
        else
          addOrders(Order.fromOrderAttached(orderId, event) :: Nil, allowClosedPlan = true)

      case _ =>
        super.applyOrderEvent(orderId, event)

  def applyKeyedEvent(keyedEvent: KeyedEvent[Event]): Checked[AgentState] =
    keyedEvent match
      case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        applyOrderEvent(orderId, event)

      case KeyedEvent(_, _: AgentEvent.AgentReady) =>
        Right(this)

      case KeyedEvent(_, AgentEvent.AgentShutDown) =>
        Right(this)

      case KeyedEvent(orderWatchPath: OrderWatchPath, event: OrderWatchEvent) =>
        fw.applyOrderWatchEvent(orderWatchPath <-: event)

      case KeyedEvent(_: NoKey, event: BasicItemEvent.ForDelegate) =>
        event match
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
            for o <- idToWorkflow.insert(workflow.id -> workflow.reduceForAgent(agentPath)) yield
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

          case ItemAttachedToMe(subagentItem: SubagentItem) =>
            // May replace an existing SubagentItem
            Right(copy(
              keyToUnsignedItemState_ = keyToUnsignedItemState_.updated(subagentItem.id,
                keyTo(SubagentItemState)
                  .get(subagentItem.id)
                  .match {
                    case None => SubagentItemState.initial(subagentItem)
                    case Some(subagentState) => subagentState.copy(subagentItem = subagentItem)
                  })))

          case ItemAttachedToMe(agentRef: AgentRef) if agentRef.path == meta.agentPath =>
            Right(copy(
              keyToUnsignedItemState_ =
                keyToUnsignedItemState_.updated(agentRef.path, agentRef.toInitialItemState),
              meta = meta.copy(
                directors = agentRef.directors)))

          case ItemAttachedToMe(item: UnsignedItem) =>
            item match
              case _: AgentRef |
                   _: WorkflowPathControl |
                   _: WorkflowControl |
                   _: Calendar |
                   _: SubagentBundle |
                   _: AgentRef |
                   _: SubagentItem =>
                // May replace an existing Item
                Right(copy(
                  keyToUnsignedItemState_ =
                    keyToUnsignedItemState_.updated(item.key, item.toInitialItemState)))

              case _ => eventNotApplicable(keyedEvent)

          case ItemDetached(itemKey, meta.agentPath) =>
            itemKey match
              case WorkflowId.as(workflowId) =>
                for _ <- idToWorkflow.checked(workflowId) yield
                  val updatedIdToWorkflow = idToWorkflow - workflowId
                  copy(
                    keyToSignedItem = keyToSignedItem - workflowId,
                    idToWorkflow = updatedIdToWorkflow)

              case path: OrderWatchPath =>
                fw.detach(path)

              case path: JobResourcePath =>
                for _ <- pathToJobResource.checked(path) yield
                  copy(
                    keyToSignedItem = keyToSignedItem - path,
                    pathToJobResource = pathToJobResource - path)

              case itemKey: UnsignedItemKey =>
                itemKey match
                  case _: AgentPath =>
                    // The Controller detaches all attached Items.
                    // But without its AgentRef, AgentOrderKeeper may complain about missing
                    // AgentRef.processLimit, when trying to start a job (short before shutdown).
                    // So we keep our AgentRef silently.
                    Right(this)

                  case _ =>
                    itemKey match
                      case _: WorkflowPathControlPath | WorkflowControlId.as(_) |
                           _: CalendarPath |
                           _: SubagentId | _: SubagentBundleId =>
                    for _ <- keyToUnsignedItemState_.checked(itemKey) yield
                          copy(
                            keyToUnsignedItemState_ = keyToUnsignedItemState_ - itemKey)

              case _ => applyStandardEvent(keyedEvent)

          case ItemDetachingFromMe(id: SubagentId) =>
            for subagentItemState <- keyTo(SubagentItemState).checked(id) yield
              copy(
                keyToUnsignedItemState_ = keyToUnsignedItemState_.updated(id,
                  subagentItemState.copy(isDetaching = true)))

          case _ => applyStandardEvent(keyedEvent)

      case KeyedEvent(subagentId: SubagentId, event: SubagentItemStateEvent) =>
        event match
          case SubagentShutdown if !keyToUnsignedItemState_.contains(subagentId) =>
            // May arrive when SubagentItem has been deleted
            Right(this)

          case _ =>
            for
              subagentItemState <- keyTo(SubagentItemState).checked(subagentId)
              subagentItemState <- subagentItemState.applyEvent(event)
            yield copy(
              keyToUnsignedItemState_ = keyToUnsignedItemState_.updated(subagentId, subagentItemState))

      case KeyedEvent(_: NoKey, AgentDedicated(directors, agentPath, agentRunId, controllerId, controllerRunId)) =>
        if meta.controllerRunId.exists(o => controllerRunId.exists(_ != o)) then
          Left(Problem("Duplicate AgentDedicated event for different ControllerRunId"))
        else
          Right(copy(meta = meta.copy(
            agentPath = agentPath,
            agentRunId = agentRunId,
            controllerId = controllerId,
            controllerRunId = controllerRunId,
            directors = directors)))
      case _ => applyStandardEvent(keyedEvent)

  def keyToUnsignedItemState: MapView[UnsignedItemKey, UnsignedItemState] =
    keyToUnsignedItemState_.view

  def idToSubagentItemState: MapView[SubagentId, SubagentItemState] =
    keyTo(SubagentItemState)

  protected def pathToFileWatchState = keyTo(FileWatchState)

  protected def updateFileWatchStates(
    fileWatchStates: Seq[FileWatchState],
    remove: Seq[OrderWatchPath])
  : Checked[AgentState] =
    update(
      addItemStates = fileWatchStates,
      removeUnsignedSimpleItems = remove)

  protected def addOrders(orders: Seq[Order[Order.State]] = Nil, allowClosedPlan: Boolean)
  : Checked[AgentState] =
    // allowClosedPlan is ignored because the Agent does not know Plans
    checkOrdersDoNotExist(orders.view.map(_.id)).flatMap: _ =>
      Right(copy(
        idToOrder = idToOrder ++ orders.view.map(o => o.id -> o)))

  protected def update_(
    updateOrders: Seq[Order[Order.State]] = Nil,
    removeOrders: Seq[OrderId] = Nil,
    externalVanishedOrders: Seq[Order[Order.State]] = Nil,
    addItemStates: Seq[UnsignedSimpleItemState] = Nil,
    removeUnsignedSimpleItems: Seq[UnsignedSimpleItemPath] = Nil)
  : Checked[AgentState] =
    if isStrict && !addItemStates.forall(o => allowedItemStates(o.companion)) then
      Left(Problem.pure("Unsupported InventoryItemState"))
    else
      Right(copy(
        idToOrder = idToOrder
          -- removeOrders
          ++ updateOrders.view.map(o => o.id -> o),
        keyToUnsignedItemState_ = keyToUnsignedItemState_
          -- removeUnsignedSimpleItems
          ++ addItemStates.view.map(o => o.path -> o)))

  def agentPath: AgentPath =
    meta.agentPath

  lazy val keyToItem: MapView[InventoryItemKey, InventoryItem] =
    new MapView[InventoryItemKey, InventoryItem]:
      def get(itemKey: InventoryItemKey): Option[InventoryItem] =
        itemKey match
          case path: JobResourcePath => pathToJobResource.get(path)
          case WorkflowId.as(id) => idToWorkflow.get(id)
          case itemKey: UnsignedItemKey => keyToUnsignedItemState_.get(itemKey).map(_.item)

      def iterator: Iterator[(InventoryItemKey, InventoryItem)] =
        pathToJobResource.iterator ++
          idToWorkflow.iterator ++
          keyToUnsignedItemState.mapValues(_.item).iterator

  def keyToSigned[I <: SignableItem](I: SignableItem.Companion[I]): MapView[I.Key, Signed[I]] =
    new MapView[I.Key, Signed[I]]:
      def get(key: I.Key) =
        keyToSignedItem.get(key).asInstanceOf[Option[Signed[I]]]

      def iterator =
        keyToSignedItem.iterator.collect:
          case pair @ (_, Signed(item, _)) if item.companion eq I =>
            pair.asInstanceOf[(I.Key, Signed[I])]

      //? override def values =
      //  keyToSignedItem.values.view.collect {
      //    case signed @ Signed(item: I, _) if I.cls.isAssignableFrom(item.getClass) =>
      //      signed.asInstanceOf[Signed[I]]
      //  }

  def workflowPathToId(workflowPath: WorkflowPath): Checked[WorkflowId] =
    Left(Problem.pure("workflowPathToId is not available at Agent"))

  def orders: Iterable[Order[Order.State]] = idToOrder.values

  def clusterNodeIdToName(nodeId: NodeId): Checked[NodeName] =
    if !isDedicated then
      Left(Problem("clusterNodeToUserAndPassword but Agent has not been dedicated"))
    else
      meta.clusterNodeIdToSubagentId(nodeId).flatMap(_.toNodeName)

  def clusterNodeToUserId(nodeId: NodeId): Checked[UserId] =
    if !isDedicated then
      Left(Problem("clusterNodeToUserId but Agent has not been dedicated"))
    else
      for
        subagentId <- meta.clusterNodeIdToSubagentId(nodeId)
        userId <- subagentId.toUserId
      yield userId


object AgentState
extends
  EventDrivenStateView.Companion[AgentState],
  ClusterableState.Companion[AgentState],
  ItemContainer.Companion[AgentState]:

  val empty: AgentState =
    AgentState(EventId.BeforeFirst, SnapshotableState.Standards.empty,
      AgentMetaState.empty,
      Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  private val allowedItemStates: Set[InventoryItemState.AnyCompanion] =
    Set(AgentRefState, SubagentItemState, FileWatchState)

  def newRecoverer() = new AgentStateRecoverer

  protected val inventoryItems = Vector(
    AgentRef, SubagentItem, SubagentBundle,
    FileWatch, JobResource, Calendar, Workflow, WorkflowPathControl, WorkflowControl)

  final case class AgentMetaState(
    directors: Seq[SubagentId],
    agentPath: AgentPath,
    agentRunId: AgentRunId,
    controllerId: ControllerId,
    controllerRunId: Option[ControllerRunId]/*COMPATIBLE None until v2.5, Some since v2.6*/):
    def clusterNodeIdToSubagentId(nodeId: NodeId): Checked[SubagentId]=
      if directors.sizeIs < 2 then
        Left(Problem("Agent has not enough directors to be a cluster"))
      else
        nodeId match
          case NodeId.primary => Right(directors(0))
          case NodeId.backup => Right(directors(1))
          case nodeId => Left(Problem(s"🔥 Unexpected $nodeId"))
  object AgentMetaState:
    val empty: AgentMetaState =
      AgentMetaState(
        Vector.empty,
        AgentPath.empty,
        AgentRunId.empty,
        ControllerId("NOT-YET-INITIALIZED"),
        None)

    implicit val jsonEncoder: Encoder.AsObject[AgentMetaState] = deriveEncoder
    implicit val jsonDecoder: Decoder[AgentMetaState] =
      c => for
        directors <- c.get[Option[SubagentId]]("subagentId").flatMap:
          case None => c.get[Option[Seq[SubagentId]]]("directors").map(_.toVector.flatten)
          case Some(subagentId) => Right(Seq(subagentId))
        agentPath <- c.get[AgentPath]("agentPath")
        agentRunId <- c.get[AgentRunId]("agentRunId")
        controllerId <- c.get[ControllerId]("controllerId")
        controllerRunId <- c.get[Option[ControllerRunId]]("controllerRunId")
      yield AgentMetaState(directors, agentPath, agentRunId, controllerId, controllerRunId)

  val snapshotObjectJsonCodec: TypedJsonCodec[Any] =
    TypedJsonCodec[Any](
      Subtype[JournalState],
      Subtype[ClusterStateSnapshot],
      Subtype[AgentMetaState],
      Workflow.subtype,
      Subtype[SubagentItemState](aliases = Seq("SubagentRefState")),
      Subtype[Order[Order.State]],
      Subtype[FileWatchState.Snapshot],
      Subtype(SignedItemAdded.jsonCodec(using this)),  // For Repo and SignedItemAdded
      Subtype(signableSimpleItemJsonCodec),
      Subtype(unsignedItemJsonCodec),
      Subtype[BasicItemEvent])

  implicit val keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    KeyedEventTypedJsonCodec[Event](
      KeyedSubtype[JournalEvent],
      KeyedSubtype[ClusterEvent],
      KeyedSubtype[SubagentItemStateEvent],
      KeyedSubtype[OrderEvent],
      KeyedSubtype[AgentEvent],
      KeyedSubtype[InventoryItemEvent],
      KeyedSubtype[OrderWatchEvent],
      KeyedSubtype.singleEvent[ServerMeteringEvent])
