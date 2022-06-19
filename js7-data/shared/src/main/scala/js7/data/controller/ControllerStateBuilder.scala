package js7.data.controller

import js7.base.crypt.Signed
import js7.base.problem.Checked
import js7.base.problem.Checked._
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.utils.Collections.implicits._
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.agent.{AgentPath, AgentRef, AgentRefState, AgentRefStateEvent}
import js7.data.board.BoardEvent.{NoticeDeleted, NoticePosted}
import js7.data.board.{Board, BoardPath, BoardState, Notice}
import js7.data.calendar.{Calendar, CalendarState}
import js7.data.cluster.{ClusterEvent, ClusterStateSnapshot}
import js7.data.controller.ControllerEvent.{ControllerShutDown, ControllerTestEvent}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, EventDrivenState, JournalEvent, JournalState, KeyedEvent, SnapshotableState, SnapshotableStateBuilder, Stamped}
import js7.data.item.BasicItemEvent.{ItemAttachedStateEvent, ItemDeleted, ItemDeletionMarked}
import js7.data.item.SignedItemEvent.{SignedItemAdded, SignedItemChanged}
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemChanged}
import js7.data.item.{BasicItemEvent, ClientAttachments, InventoryItemEvent, InventoryItemKey, Repo, SignableSimpleItem, SignableSimpleItemPath, SignedItemEvent, UnsignedSimpleItem, UnsignedSimpleItemEvent, UnsignedSimpleItemPath, UnsignedSimpleItemState, VersionedEvent}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockState}
import js7.data.order.OrderEvent.{OrderAddedX, OrderNoticesExpected}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{OrderWatch, OrderWatchEvent, OrderWatchPath, OrderWatchState, OrderWatchStateHandler}
import js7.data.state.EventDrivenStateView
import js7.data.state.WorkflowAndOrderRecovering.followUpRecoveredWorkflowsAndOrders
import js7.data.subagent.SubagentItemStateEvent.SubagentShutdown
import js7.data.subagent.{SubagentId, SubagentItem, SubagentItemState, SubagentItemStateEvent, SubagentSelection, SubagentSelectionState}
import js7.data.workflow.{Workflow, WorkflowControlEvent, WorkflowControlState, WorkflowControlStateHandler, WorkflowId, WorkflowPath}
import scala.collection.mutable

final class ControllerStateBuilder
extends SnapshotableStateBuilder[ControllerState]
with EventDrivenStateView[ControllerStateBuilder, Event]
with OrderWatchStateHandler[ControllerStateBuilder]
with WorkflowControlStateHandler[ControllerStateBuilder]
{
  protected val S = ControllerState
  val companion = ControllerStateBuilder

  private var standards: SnapshotableState.Standards = SnapshotableState.Standards.empty
  private var controllerMetaState = ControllerMetaState.Undefined
  private var repo = Repo.empty
  private val _idToOrder = mutable.Map.empty[OrderId, Order[Order.State]]
  private val _pathToItemState = mutable.Map.empty[UnsignedSimpleItemPath, UnsignedSimpleItemState]
  private var agentAttachments = ClientAttachments.empty[AgentPath]
  private val _pathToWorkflowControlState = mutable.Map.empty[WorkflowPath, WorkflowControlState]
  private val deletionMarkedItems = mutable.Set[InventoryItemKey]()
  private val pathToSignedSimpleItem = mutable.Map.empty[SignableSimpleItemPath, Signed[SignableSimpleItem]]

  val isAgent = false
  def idToOrder = _idToOrder
  def pathToWorkflowControlState = _pathToWorkflowControlState.view

  lazy val idToWorkflow: PartialFunction[WorkflowId, Workflow] =
    new PartialFunction[WorkflowId, Workflow] {
      def isDefinedAt(workflowId: WorkflowId) =
        repo.idToSigned[Workflow](workflowId).isRight

      def apply(workflowId: WorkflowId): Workflow =
        repo.idToSigned[Workflow](workflowId).orThrow.value

      override def applyOrElse[K <: WorkflowId, V >: Workflow](workflowId: K, default: K => V): V =
        repo.idToSigned[Workflow](workflowId)
          .fold(_ => default(workflowId), _.value)
    }

  def workflowPathToId(workflowPath: WorkflowPath) =
    repo.pathToId(workflowPath)
      .toRight(UnknownKeyProblem("WorkflowPath", workflowPath.string))

  val pathToItemState = _pathToItemState.view

  def controllerId = controllerMetaState.controllerId

  def orders = _idToOrder.values

  def keyToItem =
    throw new NotImplementedError("ControllerStateBuilder.keyToItem")

  def pathToJobResource = keyTo(JobResource)

  protected def onInitializeState(state: ControllerState): Unit = {
    standards = state.standards
    controllerMetaState = state.controllerMetaState
    repo = state.repo
    _idToOrder ++= state.idToOrder
    _pathToItemState ++= state.pathToItemState_
    pathToSignedSimpleItem ++= state.pathToSignedSimpleItem
    agentAttachments = state.agentAttachments
    _pathToWorkflowControlState ++= state.pathToWorkflowControlState_
    deletionMarkedItems ++= state.deletionMarkedItems
  }

  protected def onAddSnapshotObject = {
    case order: Order[Order.State] =>
      _idToOrder.insert(order.id, order)

      order.state match {
        case Order.ExpectingNotice(noticeId) =>
          val boardState = workflowPositionToBoardState(order.workflowPosition).orThrow
          _pathToItemState(boardState.path) = boardState.addExpectation(noticeId, order.id).orThrow

          // Change ExpectingNotice (Orders of v2.3) to ExpectingNotices
          _idToOrder.update(order.id, order.copy(
            state = Order.ExpectingNotices(Vector(
              OrderNoticesExpected.Expected(boardState.path, noticeId)))))

        case Order.ExpectingNotices(expectedSeq) =>
          _pathToItemState ++= expectedSeq
            .map(expected => expected.boardPath ->
              pathTo(BoardState)
                .checked(expected.boardPath)
                .flatMap(_.addExpectation(expected.noticeId, order.id))
                .orThrow)

        case _ =>
      }

    case event: VersionedEvent =>
      repo = repo.applyEvent(event).orThrow

    case agentRefState: AgentRefState =>
      val (agentRef, maybeSubagentItem) = agentRefState.agentRef.convertFromV2_1.orThrow

      _pathToItemState.insert(agentRef.path, agentRefState.copy(agentRef = agentRef))
      for (subagentItem <- maybeSubagentItem) {
        _pathToItemState.insert(subagentItem.id, SubagentItemState.initial(subagentItem))
      }

    case notice: Notice =>
      _pathToItemState(notice.boardPath) = pathTo(BoardState)(notice.boardPath)
        .addNotice(notice).orThrow

    case signedItemAdded: SignedItemAdded =>
      onSignedItemAdded(signedItemAdded)

    case UnsignedSimpleItemAdded(orderWatch: OrderWatch) =>
      ow.addOrderWatch(orderWatch.toInitialItemState).orThrow

    case itemState: UnsignedSimpleItemState =>
      _pathToItemState.insert(itemState.path, itemState)

    case item: UnsignedSimpleItem =>
      _pathToItemState.insert(item.path, item.toInitialItemState)

    case workflowControlState: WorkflowControlState =>
      updateWorkflowControlState(workflowControlState)

    case snapshot: OrderWatchState.ExternalOrderSnapshot =>
      ow.applySnapshot(snapshot).orThrow

    case event: ItemAttachedStateEvent =>
      agentAttachments = agentAttachments.applyEvent(event).orThrow

    case ItemDeletionMarked(itemKey) =>
      deletionMarkedItems += itemKey

    case o: ControllerMetaState =>
      controllerMetaState = o

    case o: JournalState =>
      standards = standards.copy(journalState = o)

    case ClusterStateSnapshot(o) =>
      standards = standards.copy(clusterState = o)
  }

  override protected def onOnAllSnapshotsAdded() = {
    val (added, deleted) = followUpRecoveredWorkflowsAndOrders(repo.idTo[Workflow], _idToOrder.toMap)
    _idToOrder ++= added
    _idToOrder --= deleted
    ow.finishRecovery.orThrow
  }

  protected def onAddEvent = {
    case Stamped(_, _, keyedEvent) => applyEventMutable(keyedEvent)
  }

  override def applyEvent(keyedEvent: KeyedEvent[Event]) = {
    applyEventMutable(keyedEvent)
    Right(this)
  }

  private def applyEventMutable(keyedEvent: KeyedEvent[Event]): Unit =
    keyedEvent match {
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
        event match {
          case event: UnsignedSimpleItemEvent =>
            event match {
              case UnsignedSimpleItemAdded(item) =>
                item match {
                  case addedAgentRef: AgentRef =>
                    val (agentRef, maybeSubagentItem) = addedAgentRef.convertFromV2_1.orThrow

                    _pathToItemState.insert(agentRef.path, agentRef.toInitialItemState)
                    for (subagentItem <- maybeSubagentItem) {
                      _pathToItemState.insert(subagentItem.id, SubagentItemState.initial(subagentItem))
                    }

                  case orderWatch: OrderWatch =>
                    ow.addOrderWatch(orderWatch.toInitialItemState).orThrow

                  case item: UnsignedSimpleItem =>
                    _pathToItemState.insert(item.path, item.toInitialItemState)
                }

              case UnsignedSimpleItemChanged(item) =>
                item match {
                  case lock: Lock =>
                    _pathToItemState(lock.path) = pathTo(LockState)(lock.path).copy(
                      lock = lock)

                  case changedAgentRef: AgentRef =>
                    val (agentRef, maybeSubagentItem) = changedAgentRef.convertFromV2_1.orThrow

                    _pathToItemState(agentRef.path) = pathTo(AgentRefState)(agentRef.path).copy(
                      agentRef = agentRef)

                    for (subagentItem <- maybeSubagentItem) {
                      _pathToItemState.updateWith(subagentItem.id) {
                        case None =>
                          Some(SubagentItemState.initial(subagentItem))

                        case Some(previous: SubagentItemState) =>
                          Some(previous.copy(
                            subagentItem = previous.item.updateUri(subagentItem.uri)))

                        case _ => sys.error("No SubagentItemState")
                      }
                    }

                  case selection: SubagentSelection =>
                    _pathToItemState(selection.id) = SubagentSelectionState(selection)

                  case subagentItem: SubagentItem =>
                    _pathToItemState(subagentItem.id) =
                      pathTo(SubagentItemState)(subagentItem.id).copy(
                        subagentItem = subagentItem)

                  case orderWatch: OrderWatch =>
                    ow.changeOrderWatch(orderWatch).orThrow

                  case board: Board =>
                    _pathToItemState.update(
                      board.path,
                      pathTo(BoardState)
                        .checked(board.path)
                        .map(boardState => boardState.copy(
                          board = board))
                        .orThrow)

                  case calendar: Calendar =>
                    _pathToItemState.update(calendar.path, CalendarState(calendar))
                }
            }

          case event: SignedItemEvent =>
            event match {
              case event: SignedItemAdded =>
                onSignedItemAdded(event)

              case SignedItemChanged(Signed(item, signedString)) =>
                item match {
                  case jobResource: JobResource =>
                    pathToSignedSimpleItem.update(jobResource.path, Signed(jobResource, signedString))
                }
            }

          case event: BasicItemEvent.ForClient =>
            event match {
              case event: ItemAttachedStateEvent =>
                agentAttachments = agentAttachments.applyEvent(event).orThrow

              case ItemDeletionMarked(itemKey) =>
                deletionMarkedItems += itemKey

              case event: ItemDeleted =>
                deletionMarkedItems -= event.key
                agentAttachments = agentAttachments.applyItemDeleted(event)

                event.key match {
                  case WorkflowId.as(workflowId) =>
                    repo = repo.deleteItem(workflowId).orThrow
                    if (!repo.pathToItems(Workflow).contains(workflowId.path)) {
                      _pathToWorkflowControlState -= workflowId.path
                    }

                  case jobResourcePath: JobResourcePath =>
                    pathToSignedSimpleItem -= jobResourcePath

                  case path: OrderWatchPath =>
                    ow.removeOrderWatch(path)

                  case path: UnsignedSimpleItemPath =>
                    _pathToItemState -= path
                }
            }
        }

      case KeyedEvent(path: AgentPath, event: AgentRefStateEvent) =>
        _pathToItemState.update(path, pathTo(AgentRefState)(path).applyEvent(event).orThrow)

      case KeyedEvent(id: SubagentId, event: SubagentItemStateEvent) =>
        event match {
          case SubagentShutdown if !_pathToItemState.contains(id) =>
            // May arrive when SubagentItem has been deleted

          case _ =>
          _pathToItemState.update(id, pathTo(SubagentItemState)(id).applyEvent(event).orThrow)
        }

      case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        super.applyOrderEvent(orderId, event).orThrow

      case KeyedEvent(orderWatchPath: OrderWatchPath, event: OrderWatchEvent) =>
        ow.onOrderWatchEvent(orderWatchPath <-: event).orThrow

      case KeyedEvent(boardPath: BoardPath, NoticePosted(notice)) =>
        for (boardState <- pathTo(BoardState).get(boardPath)) {
          _pathToItemState(boardState.path) =
            boardState.addNotice(notice.toNotice(boardState.path)).orThrow
        }

      case KeyedEvent(boardPath: BoardPath, NoticeDeleted(noticeId)) =>
        for (boardState <- pathTo(BoardState).get(boardPath)) {
          _pathToItemState(boardState.path) = boardState.removeNotice(noticeId).orThrow
        }

      case KeyedEvent(workflowPath: WorkflowPath, event: WorkflowControlEvent) =>
        applyWorkflowControlEvent(workflowPath, event).orThrow

      case KeyedEvent(_, _: ControllerShutDown) =>
      case KeyedEvent(_, ControllerTestEvent) =>

      case KeyedEvent(_: NoKey, event: JournalEvent) =>
        standards = standards.copy(
          journalState = standards.journalState.applyEvent(event))

      case KeyedEvent(_: NoKey, event: ClusterEvent) =>
        standards = standards.copy(
          clusterState = standards.clusterState.applyEvent(event).orThrow)

      case _ => eventNotApplicable(keyedEvent).orThrow
    }

  override protected def addOrder(orderId: OrderId, orderAdded: OrderAddedX) = {
    _idToOrder.insert(orderId, Order.fromOrderAdded(orderId, orderAdded))
    ow.onOrderAdded(orderId <-: orderAdded).orThrow
    Right(this)
  }

  override protected def deleteOrder(order: Order[Order.State]) = {
    for (order <- _idToOrder.remove(order.id)) {
      for (externalOrderKey <- order.externalOrderKey)
        ow.onOrderDeleted(externalOrderKey, order.id).orThrow
    }
    Right(this)
  }

  private def onSignedItemAdded(added: SignedItemEvent.SignedItemAdded): Unit =
    added.signed.value match {
      case jobResource: JobResource =>
        pathToSignedSimpleItem.insert(jobResource.path, Signed(jobResource, added.signedString))
    }

  protected def pathToOrderWatchState = pathTo(OrderWatchState)

  protected def updateWorkflowControlState(state: WorkflowControlState) = {
    _pathToWorkflowControlState(state.workflowPath) = state
    this
  }

  protected def updateOrderWatchStates(
    orderWatchStates: Iterable[OrderWatchState],
    remove: Iterable[OrderWatchPath]
  ) = update(addItemStates = orderWatchStates, removeItemStates = remove)

  protected def update(
    addOrders: Iterable[Order[Order.State]],
    removeOrders: Iterable[OrderId],
    addItemStates: Iterable[UnsignedSimpleItemState],
    removeItemStates: Iterable[UnsignedSimpleItemPath])
  : Checked[ControllerStateBuilder] = {
    _idToOrder --= removeOrders
    _idToOrder ++= addOrders.map(o => o.id -> o)
    _pathToItemState --= removeItemStates
    _pathToItemState ++= addItemStates.map(o => o.path -> o)
    Right(this)
  }

  def result() =
    ControllerState(
      eventId = eventId,
      standards,
      controllerMetaState,
      _pathToItemState.toMap,
      repo,
      pathToSignedSimpleItem.toMap,
      agentAttachments,
      _pathToWorkflowControlState.toMap,
      deletionMarkedItems.toSet,
      _idToOrder.toMap)

  def journalState = standards.journalState

  def clusterState = standards.clusterState
}

object ControllerStateBuilder extends EventDrivenState.Companion[ControllerStateBuilder, Event]
