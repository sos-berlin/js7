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
import js7.data.calendar.{Calendar, CalendarPath}
import js7.data.cluster.{ClusterEvent, ClusterStateSnapshot}
import js7.data.controller.ControllerEvent.{ControllerShutDown, ControllerTestEvent}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, EventDrivenState, JournalEvent, JournalState, KeyedEvent, SnapshotableState, SnapshotableStateBuilder, Stamped}
import js7.data.item.BasicItemEvent.{ItemAttachedStateEvent, ItemDeleted, ItemDeletionMarked}
import js7.data.item.SignedItemEvent.{SignedItemAdded, SignedItemChanged}
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemChanged}
import js7.data.item.{BasicItemEvent, ClientAttachments, InventoryItemEvent, InventoryItemKey, Repo, SignableSimpleItem, SignableSimpleItemPath, SignedItemEvent, UnsignedSimpleItemEvent, VersionedEvent, VersionedItemId_}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.order.OrderEvent.{OrderAddedX, OrderNoticesExpected}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{AllOrderWatchesState, OrderWatch, OrderWatchEvent, OrderWatchPath, OrderWatchState}
import js7.data.state.EventDrivenStateView
import js7.data.state.WorkflowAndOrderRecovering.followUpRecoveredWorkflowsAndOrders
import js7.data.subagent.SubagentItemStateEvent.SubagentShutdown
import js7.data.subagent.{SubagentId, SubagentItem, SubagentItemState, SubagentItemStateEvent, SubagentSelection, SubagentSelectionId}
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import scala.collection.mutable

final class ControllerStateBuilder
extends SnapshotableStateBuilder[ControllerState]
with EventDrivenStateView[ControllerStateBuilder, Event]
{
  protected val S = ControllerState
  val companion = ControllerStateBuilder

  private var standards: SnapshotableState.Standards = SnapshotableState.Standards.empty
  private var controllerMetaState = ControllerMetaState.Undefined
  private var repo = Repo.empty
  private val _idToOrder = mutable.Map.empty[OrderId, Order[Order.State]]
  private val _pathToAgentRefState = mutable.Map.empty[AgentPath, AgentRefState]
  private val _idToSubagentItemState = mutable.Map.empty[SubagentId, SubagentItemState]
  private val _idToSubagentSelection = mutable.Map.empty[SubagentSelectionId, SubagentSelection]
  private val _pathToLockState = mutable.Map.empty[LockPath, LockState]
  private val _pathToBoardState = mutable.Map.empty[BoardPath, BoardState]
  private val _pathToCalendar = mutable.Map.empty[CalendarPath, Calendar]
  private var allOrderWatchesState = AllOrderWatchesState.empty
  private var agentAttachments = ClientAttachments.empty[AgentPath]
  private val deletionMarkedItems = mutable.Set[InventoryItemKey]()
  private val pathToSignedSimpleItem = mutable.Map.empty[SignableSimpleItemPath, Signed[SignableSimpleItem]]

  val isAgent = false
  val idToOrder = _idToOrder

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

  val pathToLockState = _pathToLockState
  val pathToBoardState = _pathToBoardState
  val pathToCalendar = _pathToCalendar
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
    _pathToAgentRefState ++= state.pathToAgentRefState
    _idToSubagentItemState ++= state.idToSubagentItemState
    _idToSubagentSelection ++= state.idToSubagentSelection
    _pathToLockState ++= state.pathToLockState
    _pathToBoardState ++= state.pathToBoardState
    _pathToCalendar ++= state.pathToCalendar
    allOrderWatchesState = state.allOrderWatchesState
    pathToSignedSimpleItem ++= state.pathToSignedSimpleItem
    agentAttachments = state.agentAttachments
    deletionMarkedItems ++= state.deletionMarkedItems
  }

  protected def onAddSnapshotObject = {
    case order: Order[Order.State] =>
      _idToOrder.insert(order.id, order)

      order.state match {
        case Order.ExpectingNotice(noticeId) =>
          val boardState = workflowPositionToBoardState(order.workflowPosition).orThrow
          _pathToBoardState += boardState.path -> boardState.addExpectation(noticeId, order.id).orThrow

          // Change ExpectingNotice (Orders of v2.3) to ExpectingNotices
          _idToOrder.update(order.id, order.copy(
            state = Order.ExpectingNotices(Vector(
              OrderNoticesExpected.Expected(boardState.path, noticeId)))))

        case Order.ExpectingNotices(expectedSeq) =>
          _pathToBoardState ++= expectedSeq
            .map(expected => expected.boardPath ->
              pathToBoardState
                .checked(expected.boardPath)
                .flatMap(_.addExpectation(expected.noticeId, order.id))
                .orThrow)

        case _ =>
      }

    case event: VersionedEvent =>
      repo = repo.applyEvent(event).orThrow

    case agentRefState: AgentRefState =>
      val (agentRef, maybeSubagentItem) = agentRefState.agentRef.convertFromV2_1.orThrow

      _pathToAgentRefState.insert(agentRef.path, agentRefState.copy(agentRef = agentRef))
      for (subagentItem <- maybeSubagentItem) {
        _idToSubagentItemState.insert(subagentItem.id, SubagentItemState.initial(subagentItem))
      }

    case subagentItemState: SubagentItemState =>
      _idToSubagentItemState.insert(subagentItemState.subagentId, subagentItemState)

    case subagentSelection: SubagentSelection =>
      _idToSubagentSelection.insert(subagentSelection.id, subagentSelection)

    case lockState: LockState =>
      _pathToLockState.insert(lockState.lock.path, lockState)

    case board: Board =>
      _pathToBoardState.insert(board.path, BoardState(board))

    case calendar: Calendar =>
      _pathToCalendar.insert(calendar.path, calendar)

    case notice: Notice =>
      _pathToBoardState(notice.boardPath) = _pathToBoardState(notice.boardPath)
        .addNotice(notice).orThrow

    case signedItemAdded: SignedItemAdded =>
      onSignedItemAdded(signedItemAdded)

    case UnsignedSimpleItemAdded(orderWatch: OrderWatch) =>
      allOrderWatchesState = allOrderWatchesState.addOrderWatch(orderWatch).orThrow

    case snapshot: OrderWatchState.ExternalOrderSnapshot =>
      allOrderWatchesState = allOrderWatchesState.applySnapshot(snapshot).orThrow

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
    allOrderWatchesState = allOrderWatchesState.finishRecovery.orThrow
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
                  case lock: Lock =>
                    _pathToLockState.insert(lock.path, LockState(lock))

                  case addedAgentRef: AgentRef =>
                    val (agentRef, maybeSubagentItem) = addedAgentRef.convertFromV2_1.orThrow

                    _pathToAgentRefState.insert(agentRef.path, AgentRefState(agentRef))
                    for (subagentItem <- maybeSubagentItem) {
                      _idToSubagentItemState.insert(subagentItem.id, SubagentItemState.initial(subagentItem))
                    }

                  case subagentItem: SubagentItem =>
                    _idToSubagentItemState.insert(subagentItem.id, SubagentItemState.initial(subagentItem))

                  case selection: SubagentSelection =>
                    _idToSubagentSelection.insert(selection.id, selection)

                  case orderWatch: OrderWatch =>
                    allOrderWatchesState = allOrderWatchesState.addOrderWatch(orderWatch).orThrow

                  case board: Board =>
                    _pathToBoardState.insert(board.path, BoardState(board))

                  case calendar: Calendar =>
                    _pathToCalendar.insert(calendar.path, calendar)
                }

              case UnsignedSimpleItemChanged(item) =>
                item match {
                  case lock: Lock =>
                    _pathToLockState(lock.path) = _pathToLockState(lock.path).copy(
                      lock = lock)

                  case changedAgentRef: AgentRef =>
                    val (agentRef, maybeSubagentItem) = changedAgentRef.convertFromV2_1.orThrow

                    _pathToAgentRefState(agentRef.path) = _pathToAgentRefState(agentRef.path).copy(
                      agentRef = agentRef)

                    for (subagentItem <- maybeSubagentItem) {
                      _idToSubagentItemState.updateWith(subagentItem.id) {
                        case None =>
                          Some(SubagentItemState.initial(subagentItem))

                        case Some(previous) =>
                          Some(previous.copy(
                            subagentItem = previous.item.updateUri(subagentItem.uri)))
                      }
                    }

                  case selection: SubagentSelection =>
                    _idToSubagentSelection(selection.id) = selection

                  case subagentItem: SubagentItem =>
                    _idToSubagentItemState(subagentItem.id) = _idToSubagentItemState(subagentItem.id).copy(
                      subagentItem = subagentItem)

                  case orderWatch: OrderWatch =>
                    allOrderWatchesState = allOrderWatchesState.changeOrderWatch(orderWatch).orThrow

                  case board: Board =>
                    _pathToBoardState.update(
                      board.path,
                      _pathToBoardState
                        .checked(board.path)
                        .map(boardState => boardState.copy(
                          board = board))
                        .orThrow)

                  case calendar: Calendar =>
                    _pathToCalendar.update(calendar.path, calendar)
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
                  case id: VersionedItemId_ =>
                    repo = repo.deleteItem(id).orThrow

                  case path: OrderWatchPath =>
                    allOrderWatchesState = allOrderWatchesState.removeOrderWatch(path)

                  case path: LockPath =>
                    _pathToLockState -= path

                  case agentPath: AgentPath =>
                    _pathToAgentRefState -= agentPath

                  case subagentId: SubagentId =>
                    _idToSubagentItemState -= subagentId

                  case id: SubagentSelectionId =>
                    _idToSubagentSelection -= id

                  case boardPath: BoardPath =>
                    _pathToBoardState -= boardPath

                  case calendarPath: CalendarPath =>
                    _pathToCalendar -= calendarPath

                  case jobResourcePath: JobResourcePath =>
                    pathToSignedSimpleItem -= jobResourcePath
                }
            }
        }

      case KeyedEvent(path: AgentPath, event: AgentRefStateEvent) =>
        _pathToAgentRefState.update(path, _pathToAgentRefState(path).applyEvent(event).orThrow)


      case KeyedEvent(id: SubagentId, event: SubagentItemStateEvent) =>
        event match {
          case SubagentShutdown if !_idToSubagentItemState.contains(id) =>
            // May arrive when SubagentItem has been deleted

          case _ =>
          _idToSubagentItemState.update(id, _idToSubagentItemState(id).applyEvent(event).orThrow)
        }

      case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        super.applyOrderEvent(orderId, event).orThrow

      case KeyedEvent(orderWatchPath: OrderWatchPath, event: OrderWatchEvent) =>
        allOrderWatchesState = allOrderWatchesState.onOrderWatchEvent(orderWatchPath <-: event).orThrow

      case KeyedEvent(boardPath: BoardPath, NoticePosted(notice)) =>
        for (boardState <- _pathToBoardState.get(boardPath)) {
          _pathToBoardState(boardState.path) =
            boardState.addNotice(notice.toNotice(boardState.path)).orThrow
        }

      case KeyedEvent(boardPath: BoardPath, NoticeDeleted(noticeId)) =>
        for (boardState <- _pathToBoardState.get(boardPath)) {
          _pathToBoardState(boardState.path) = boardState.removeNotice(noticeId).orThrow
        }

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
    allOrderWatchesState = allOrderWatchesState.onOrderAdded(orderId <-: orderAdded)
      .orThrow
    Right(this)
  }

  override protected def deleteOrder(order: Order[Order.State]) = {
    for (order <- _idToOrder.remove(order.id)) {
      for (externalOrderKey <- order.externalOrderKey)
        allOrderWatchesState = allOrderWatchesState
          .onOrderDeleted(externalOrderKey, order.id)
          .orThrow
    }
    Right(this)
  }

  private def onSignedItemAdded(added: SignedItemEvent.SignedItemAdded): Unit =
    added.signed.value match {
      case jobResource: JobResource =>
        pathToSignedSimpleItem.insert(jobResource.path, Signed(jobResource, added.signedString))
    }

  protected def update(
    removeOrders: Iterable[OrderId],
    orders: Iterable[Order[Order.State]],
    lockStates: Iterable[LockState],
    boardStates: Iterable[BoardState])
  : Checked[ControllerStateBuilder] = {
    _idToOrder --= removeOrders
    _idToOrder ++= orders.map(o => o.id -> o)
    _pathToLockState ++= lockStates.map(o => o.lock.path -> o)
    _pathToBoardState ++= boardStates.map(o => o.path -> o)
    Right(this)
  }

  def result() =
    ControllerState(
      eventId = eventId,
      standards,
      controllerMetaState,
      _pathToAgentRefState.toMap,
      _idToSubagentItemState.toMap,
      _idToSubagentSelection.toMap,
      _pathToLockState.toMap,
      _pathToBoardState.toMap,
      _pathToCalendar.toMap,
      allOrderWatchesState,
      repo,
      pathToSignedSimpleItem.toMap,
      agentAttachments,
      deletionMarkedItems.toSet,
      _idToOrder.toMap)

  def journalState = standards.journalState

  def clusterState = standards.clusterState
}

object ControllerStateBuilder extends EventDrivenState.Companion[ControllerStateBuilder, Event]
