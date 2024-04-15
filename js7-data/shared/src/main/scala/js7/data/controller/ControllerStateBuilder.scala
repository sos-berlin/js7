package js7.data.controller

import js7.base.crypt.Signed
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.utils.Collections.implicits.*
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.agent.{AgentPath, AgentRef, AgentRefState, AgentRefStateEvent}
import js7.data.board.BoardEvent.{NoticeDeleted, NoticePosted}
import js7.data.board.{Board, BoardPath, BoardSnapshot, BoardState}
import js7.data.calendar.{Calendar, CalendarState}
import js7.data.cluster.{ClusterEvent, ClusterStateSnapshot}
import js7.data.controller.ControllerEvent.{ControllerShutDown, ControllerTestEvent}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, EventDrivenState, JournalEvent, JournalState, KeyedEvent, SnapshotableStateBuilder, Stamped, StandardsBuilder}
import js7.data.item.BasicItemEvent.{ItemAttachedStateEvent, ItemDeleted, ItemDeletionMarked}
import js7.data.item.SignedItemEvent.{SignedItemAdded, SignedItemChanged}
import js7.data.item.UnsignedItemEvent.{UnsignedItemAdded, UnsignedItemChanged}
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemChanged}
import js7.data.item.{BasicItemEvent, ClientAttachments, InventoryItemEvent, InventoryItemKey, Repo, SignableSimpleItem, SignableSimpleItemPath, SignedItemEvent, UnsignedItemKey, UnsignedItemState, UnsignedSimpleItem, UnsignedSimpleItemEvent, UnsignedSimpleItemPath, UnsignedSimpleItemState, VersionedControl, VersionedEvent}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockState}
import js7.data.order.OrderEvent.OrderNoticesExpected
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{OrderWatch, OrderWatchEvent, OrderWatchPath, OrderWatchState, OrderWatchStateHandler}
import js7.data.state.EventDrivenStateView
import js7.data.state.WorkflowAndOrderRecovering.followUpRecoveredWorkflowsAndOrders
import js7.data.subagent.SubagentItemStateEvent.SubagentShutdown
import js7.data.subagent.{SubagentId, SubagentItem, SubagentItemState, SubagentItemStateEvent, SubagentSelection, SubagentSelectionState}
import js7.data.workflow.{Workflow, WorkflowControl, WorkflowId, WorkflowPath, WorkflowPathControl}
import scala.collection.{MapView, mutable}

final class ControllerStateBuilder
extends SnapshotableStateBuilder[ControllerState],
  StandardsBuilder,
  EventDrivenStateView[ControllerStateBuilder, Event],
  OrderWatchStateHandler[ControllerStateBuilder]:

  protected val S = ControllerState
  val companion: ControllerStateBuilder.type = ControllerStateBuilder

  private var controllerMetaState = ControllerMetaState.Undefined
  private var repo = Repo.empty
  private val _idToOrder = mutable.Map.empty[OrderId, Order[Order.State]]
  private val _keyToUnsignedItemState = mutable.Map.empty[UnsignedItemKey, UnsignedItemState]
  private var agentAttachments = ClientAttachments.empty[AgentPath]
  private val deletionMarkedItems = mutable.Set[InventoryItemKey]()
  private val pathToSignedSimpleItem = mutable.Map.empty[SignableSimpleItemPath, Signed[SignableSimpleItem]]

  val isAgent = false

  def idToOrder: PartialFunction[OrderId, Order[Order.State]] =
    _idToOrder

  lazy val idToWorkflow: PartialFunction[WorkflowId, Workflow] =
    new PartialFunction[WorkflowId, Workflow]:
      def isDefinedAt(workflowId: WorkflowId) =
        repo.idToSigned(Workflow)(workflowId).isRight

      def apply(workflowId: WorkflowId): Workflow =
        repo.idToSigned(Workflow)(workflowId).orThrow.value

      override def applyOrElse[K <: WorkflowId, V >: Workflow](workflowId: K, default: K => V): V =
        repo.idToSigned(Workflow)(workflowId)
          .fold(_ => default(workflowId), _.value)

  def workflowPathToId(workflowPath: WorkflowPath): Checked[WorkflowId] =
    repo.pathToId(workflowPath)
      .toRight(UnknownKeyProblem("WorkflowPath", workflowPath.string))

  val keyToUnsignedItemState: MapView[UnsignedItemKey, UnsignedItemState] =
    _keyToUnsignedItemState.view

  def controllerId: ControllerId =
    controllerMetaState.controllerId

  def orders: Iterable[Order[Order.State]] =
    _idToOrder.values

  def keyToItem: Nothing =
    throw new NotImplementedError("ControllerStateBuilder.keyToItem")

  def pathToJobResource: MapView[JobResourcePath, JobResource] =
    keyToItem(JobResource)

  protected def onInitializeState(state: ControllerState): Unit =
    _standards = state.standards
    controllerMetaState = state.controllerMetaState
    repo = state.repo
    _idToOrder ++= state.idToOrder
    _keyToUnsignedItemState ++= state.keyToUnsignedItemState_
    pathToSignedSimpleItem ++= state.pathToSignedSimpleItem
    agentAttachments = state.agentAttachments
    deletionMarkedItems ++= state.deletionMarkedItems

  protected def onAddSnapshotObject =
    case order: Order[Order.State] =>
      _idToOrder.insert(order.id, order)

      order.state match
        case Order.ExpectingNotice(noticeId) =>
          val boardState = workflowPositionToBoardState(order.workflowPosition).orThrow
          _keyToUnsignedItemState(boardState.path) = boardState.addExpectation(noticeId, order.id).orThrow

          // Change ExpectingNotice (Orders of v2.3) to ExpectingNotices
          _idToOrder.update(order.id, order.copy(
            state = Order.ExpectingNotices(Vector(
              OrderNoticesExpected.Expected(boardState.path, noticeId)))))

        case Order.ExpectingNotices(expectedSeq) =>
          _keyToUnsignedItemState ++= expectedSeq
            .map(expected => expected.boardPath ->
              keyTo(BoardState)
                .checked(expected.boardPath)
                .flatMap(_.addExpectation(expected.noticeId, order.id))
                .orThrow)

        case _ =>

    case event: VersionedEvent =>
      repo = repo.applyEvent(event).orThrow

    case agentRefState: AgentRefState =>
      val (agentRef, maybeSubagentItem) = agentRefState.agentRef.convertFromV2_1.orThrow

      _keyToUnsignedItemState.insert(agentRef.path, agentRefState.copy(agentRef = agentRef))
      for subagentItem <- maybeSubagentItem do
        _keyToUnsignedItemState.insert(subagentItem.id, SubagentItemState.initial(subagentItem))

    case snapshot: BoardSnapshot =>
      _keyToUnsignedItemState(snapshot.boardPath) =
        keyTo(BoardState)(snapshot.boardPath)
          .recover(snapshot)
          .orThrow

    case signedItemAdded: SignedItemAdded =>
      onSignedItemAdded(signedItemAdded)

    case UnsignedSimpleItemAdded(orderWatch: OrderWatch) =>
      ow.addOrderWatch(orderWatch.toInitialItemState).orThrow

    case itemState: UnsignedSimpleItemState =>
      _keyToUnsignedItemState.insert(itemState.path, itemState)

    case item: UnsignedSimpleItem =>
      _keyToUnsignedItemState.insert(item.path, item.toInitialItemState)

    case item: VersionedControl =>
      _keyToUnsignedItemState.insert(item.key, item.toInitialItemState)

    case snapshot: OrderWatchState.ExternalOrderSnapshot =>
      ow.applySnapshot(snapshot).orThrow

    case event: ItemAttachedStateEvent =>
      agentAttachments = agentAttachments.applyEvent(event).orThrow

    case ItemDeletionMarked(itemKey) =>
      deletionMarkedItems += itemKey

    case o: ControllerMetaState =>
      controllerMetaState = o

    case o @ (_: JournalState | _: ClusterStateSnapshot) =>
      addStandardObject(o)

  override protected def onOnAllSnapshotsAdded(): Unit =
    val (added, deleted) = followUpRecoveredWorkflowsAndOrders(repo.idTo(Workflow), _idToOrder.toMap)
    _idToOrder ++= added
    _idToOrder --= deleted
    ow.finishRecovery.orThrow

  protected def onAddEvent =
    case Stamped(_, _, keyedEvent) => applyEventMutable(keyedEvent)

  override def applyEvent(keyedEvent: KeyedEvent[Event]): Checked[ControllerStateBuilder] =
    applyEventMutable(keyedEvent)
    Right(this)

  private def applyEventMutable(keyedEvent: KeyedEvent[Event]): Unit =
    keyedEvent match
      case KeyedEvent(_: NoKey, ControllerEvent.ControllerInitialized(controllerId, startedAt)) =>
        controllerMetaState = controllerMetaState.copy(
          controllerId = controllerId,
          initiallyStartedAt = startedAt)

      case KeyedEvent(_: NoKey, event: ControllerEvent.ControllerReady) =>
        controllerMetaState = controllerMetaState.copy(
          timezone = event.timezone)

      case KeyedEvent(_: NoKey, event: VersionedEvent) =>
        repo = repo.applyEvent(event).orThrow

      case KeyedEvent(_: NoKey, event: InventoryItemEvent) =>
        event match
          case event: UnsignedSimpleItemEvent =>
            event match
              case UnsignedSimpleItemAdded(item) =>
                item match
                  case addedAgentRef: AgentRef =>
                    val (agentRef, maybeSubagentItem) = addedAgentRef.convertFromV2_1.orThrow

                    _keyToUnsignedItemState.insert(agentRef.path, agentRef.toInitialItemState)
                    for subagentItem <- maybeSubagentItem do
                      _keyToUnsignedItemState.insert(subagentItem.id, SubagentItemState.initial(subagentItem))

                  case orderWatch: OrderWatch =>
                    ow.addOrderWatch(orderWatch.toInitialItemState).orThrow

                  case item: UnsignedSimpleItem =>
                    _keyToUnsignedItemState.insert(item.path, item.toInitialItemState)

              case UnsignedSimpleItemChanged(item) =>
                item match
                  case lock: Lock =>
                    _keyToUnsignedItemState(lock.path) = keyTo(LockState)(lock.path).copy(
                      lock = lock)

                  case changedAgentRef: AgentRef =>
                    val (agentRef, maybeSubagentItem) = changedAgentRef.convertFromV2_1.orThrow

                    _keyToUnsignedItemState(agentRef.path) = keyTo(AgentRefState)(agentRef.path).copy(
                      agentRef = agentRef)

                    for subagentItem <- maybeSubagentItem do
                      _keyToUnsignedItemState.updateWith(subagentItem.id):
                        case None =>
                          Some(SubagentItemState.initial(subagentItem))

                        case Some(previous: SubagentItemState) =>
                          Some(previous.copy(
                            subagentItem = previous.item.updateUri(subagentItem.uri)))

                        case _ => sys.error("No SubagentItemState")

                  case selection: SubagentSelection =>
                    _keyToUnsignedItemState(selection.id) = SubagentSelectionState(selection)

                  case subagentItem: SubagentItem =>
                    _keyToUnsignedItemState(subagentItem.id) =
                      keyTo(SubagentItemState)(subagentItem.id).copy(
                        subagentItem = subagentItem)

                  case orderWatch: OrderWatch =>
                    ow.changeOrderWatch(orderWatch).orThrow

                  case board: Board =>
                    _keyToUnsignedItemState.update(
                      board.path,
                      keyTo(BoardState)
                        .checked(board.path)
                        .map(boardState => boardState.copy(
                          board = board))
                        .orThrow)

                  case calendar: Calendar =>
                    _keyToUnsignedItemState.update(calendar.path, CalendarState(calendar))

                  case item: WorkflowPathControl =>
                    _keyToUnsignedItemState(item.path) = keyTo(WorkflowPathControl)(item.path)
                      .updateItem(item).orThrow

          case UnsignedItemAdded(item: VersionedControl) =>
            _keyToUnsignedItemState.insert(item.key, item.toInitialItemState)

          case UnsignedItemChanged(item: VersionedControl) =>
            item match
              case item: WorkflowControl =>
                _keyToUnsignedItemState(item.key) = keyTo(WorkflowControl)(item.key)
                  .updateItem(item).orThrow

          case event: SignedItemEvent =>
            event match
              case event: SignedItemAdded =>
                onSignedItemAdded(event)

              case SignedItemChanged(Signed(item, signedString)) =>
                item match
                  case jobResource: JobResource =>
                    pathToSignedSimpleItem.update(jobResource.path, Signed(jobResource, signedString))

          case event: BasicItemEvent.ForClient =>
            event match
              case event: ItemAttachedStateEvent =>
                agentAttachments = agentAttachments.applyEvent(event).orThrow

              case ItemDeletionMarked(itemKey) =>
                deletionMarkedItems += itemKey

              case event: ItemDeleted =>
                deletionMarkedItems -= event.key
                agentAttachments = agentAttachments.applyItemDeleted(event)

                event.key match
                  case WorkflowId.as(workflowId) =>
                    repo = repo.deleteItem(workflowId).orThrow

                  case jobResourcePath: JobResourcePath =>
                    pathToSignedSimpleItem -= jobResourcePath

                  case path: OrderWatchPath =>
                    ow.removeOrderWatch(path)

                  case itemKey: UnsignedItemKey =>
                    _keyToUnsignedItemState -= itemKey

      case KeyedEvent(path: AgentPath, event: AgentRefStateEvent) =>
        _keyToUnsignedItemState.update(path, keyTo(AgentRefState)(path).applyEvent(event).orThrow)

      case KeyedEvent(id: SubagentId, event: SubagentItemStateEvent) =>
        event match
          case SubagentShutdown if !_keyToUnsignedItemState.contains(id) =>
            // May arrive when SubagentItem has been deleted

          case _ =>
          _keyToUnsignedItemState.update(id, keyTo(SubagentItemState)(id).applyEvent(event).orThrow)

      case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        super.applyOrderEvent(orderId, event).orThrow

      case KeyedEvent(orderWatchPath: OrderWatchPath, event: OrderWatchEvent) =>
        ow.onOrderWatchEvent(orderWatchPath <-: event).orThrow

      case KeyedEvent(boardPath: BoardPath, NoticePosted(notice)) =>
        for boardState <- keyTo(BoardState).get(boardPath) do
          _keyToUnsignedItemState(boardState.path) =
            boardState.addNotice(notice.toNotice(boardState.path)).orThrow

      case KeyedEvent(boardPath: BoardPath, NoticeDeleted(noticeId)) =>
        for boardState <- keyTo(BoardState).get(boardPath) do
          _keyToUnsignedItemState(boardState.path) = boardState.removeNotice(noticeId).orThrow

      case KeyedEvent(_, _: ControllerShutDown) =>
      case KeyedEvent(_, ControllerTestEvent) =>

      case KeyedEvent(_: NoKey, event: JournalEvent) =>
        _standards = _standards.copy(
          journalState = _standards.journalState.applyEvent(event))

      case KeyedEvent(_: NoKey, event: ClusterEvent) =>
        _standards = _standards.copy(
          clusterState = _standards.clusterState.applyEvent(event).orThrow)

      case _ => eventNotApplicable(keyedEvent).orThrow

  override protected def addOrder(order: Order[Order.State]) =
    _idToOrder.insert(order.id, order)
    ow.onOrderAdded(order).orThrow
    Right(this)

  override protected def deleteOrder(order: Order[Order.State]) =
    for order <- _idToOrder.remove(order.id) do
      for externalOrderKey <- order.externalOrderKey do
        ow.onOrderDeleted(externalOrderKey, order.id).orThrow
    Right(this)

  private def onSignedItemAdded(added: SignedItemEvent.SignedItemAdded): Unit =
    added.signed.value match
      case jobResource: JobResource =>
        pathToSignedSimpleItem.insert(jobResource.path, Signed(jobResource, added.signedString))

  protected def pathToOrderWatchState = keyTo(OrderWatchState)

  protected def updateOrderWatchStates(
    orderWatchStates: Iterable[OrderWatchState],
    remove: Iterable[OrderWatchPath])
  : Checked[ControllerStateBuilder] =
    update(addItemStates = orderWatchStates, removeItemStates = remove)

  protected def update(
    addOrders: Iterable[Order[Order.State]],
    removeOrders: Iterable[OrderId],
    addItemStates: Iterable[UnsignedSimpleItemState],
    removeItemStates: Iterable[UnsignedSimpleItemPath])
  : Checked[ControllerStateBuilder] =
    _idToOrder --= removeOrders
    _idToOrder ++= addOrders.map(o => o.id -> o)
    _keyToUnsignedItemState --= removeItemStates
    _keyToUnsignedItemState ++= addItemStates.map(o => o.path -> o)
    Right(this)

  def result(): ControllerState =
    ControllerState(
      eventId = eventId,
      _standards,
      controllerMetaState,
      _keyToUnsignedItemState.toMap,
      repo,
      pathToSignedSimpleItem.toMap,
      agentAttachments,
      deletionMarkedItems.toSet,
      _idToOrder.toMap
    ).finish


object ControllerStateBuilder extends EventDrivenState.Companion[ControllerStateBuilder, Event]
