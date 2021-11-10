package js7.data.controller

import js7.base.crypt.Signed
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
import js7.data.event.{JournalEvent, JournalState, KeyedEvent, SnapshotableState, SnapshotableStateBuilder, Stamped}
import js7.data.item.BasicItemEvent.{ItemAttachedStateEvent, ItemDeleted, ItemDeletionMarked}
import js7.data.item.ItemAttachedState.{Detached, NotDetached}
import js7.data.item.SignedItemEvent.{SignedItemAdded, SignedItemChanged}
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemChanged}
import js7.data.item.{BasicItemEvent, InventoryItem, InventoryItemEvent, InventoryItemKey, ItemAttachedState, Repo, SignableSimpleItem, SignableSimpleItemPath, SignedItemEvent, UnsignedSimpleItemEvent, VersionedEvent, VersionedItemId_}
import js7.data.job.JobResource
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.order.Order.ExpectingNotice
import js7.data.order.OrderEvent.{OrderAdded, OrderAddedX, OrderCancelled, OrderCoreEvent, OrderDeleted, OrderDeletionMarked, OrderForked, OrderJoined, OrderLockEvent, OrderNoticeEvent, OrderNoticeExpected, OrderNoticePosted, OrderNoticeRead, OrderOrderAdded, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{AllOrderWatchesState, OrderWatch, OrderWatchEvent, OrderWatchPath, OrderWatchState}
import js7.data.state.StateView
import js7.data.state.WorkflowAndOrderRecovering.followUpRecoveredWorkflowsAndOrders
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import scala.collection.{MapView, mutable}

final class ControllerStateBuilder
extends SnapshotableStateBuilder[ControllerState]
with StateView
{
  protected val S = ControllerState

  private var standards: SnapshotableState.Standards = SnapshotableState.Standards.empty
  private var controllerMetaState = ControllerMetaState.Undefined
  private var repo = Repo.empty
  private val _idToOrder = mutable.Map.empty[OrderId, Order[Order.State]]
  private val _pathToAgentRefState = mutable.Map.empty[AgentPath, AgentRefState]
  private val _pathToLockState = mutable.Map.empty[LockPath, LockState]
  private val _pathToBoardState = mutable.Map.empty[BoardPath, BoardState]
  private val _pathToCalendar = mutable.Map.empty[CalendarPath, Calendar]
  private var allOrderWatchesState = AllOrderWatchesState.empty
  private val itemToAgentToAttachedState = mutable.Map.empty[InventoryItemKey, Map[AgentPath, ItemAttachedState.NotDetached]]
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

  lazy val keyToItem: MapView[InventoryItemKey, InventoryItem] =
    throw new NotImplementedError("ControllerStateBuilder.keyToItem")

  protected def onInitializeState(state: ControllerState): Unit = {
    standards = state.standards
    controllerMetaState = state.controllerMetaState
    repo = state.repo
    _idToOrder.clear()
    _idToOrder ++= state.idToOrder
    _pathToAgentRefState.clear()
    _pathToAgentRefState ++= state.pathToAgentRefState
    _pathToLockState ++= state.pathToLockState
    _pathToBoardState ++= state.pathToBoardState
    _pathToCalendar ++= state.pathToCalendar
    allOrderWatchesState = state.allOrderWatchesState
    pathToSignedSimpleItem ++= state.pathToSignedSimpleItem
    itemToAgentToAttachedState ++= state.itemToAgentToAttachedState
    deletionMarkedItems ++= state.deletionMarkedItems
  }

  protected def onAddSnapshotObject = {
    case order: Order[Order.State] =>
      _idToOrder.insert(order.id -> order)

      order.state match {
        case Order.ExpectingNotice(noticeId) =>
          val boardState = workflowPositionToBoardState(order.workflowPosition).orThrow
          _pathToBoardState += boardState.path -> boardState.addExpectation(order.id, noticeId).orThrow
        case _ =>
      }

    case event: VersionedEvent =>
      repo = repo.applyEvent(event).orThrow

    case agentRefState: AgentRefState =>
      _pathToAgentRefState.insert(agentRefState.agentPath -> agentRefState)

    case lockState: LockState =>
      _pathToLockState.insert(lockState.lock.path -> lockState)

    case board: Board =>
      _pathToBoardState.insert(board.path -> BoardState(board))

    case calendar: Calendar =>
      _pathToCalendar.insert(calendar.path -> calendar)

    case noticeSnapshot: Notice.Snapshot =>
      _pathToBoardState(noticeSnapshot.boardPath) = _pathToBoardState(noticeSnapshot.boardPath)
        .addNotice(noticeSnapshot.notice).orThrow

    case signedItemAdded: SignedItemAdded =>
      onSignedItemAdded(signedItemAdded)

    case UnsignedSimpleItemAdded(orderWatch: OrderWatch) =>
      allOrderWatchesState = allOrderWatchesState.addOrderWatch(orderWatch).orThrow

    case snapshot: OrderWatchState.ExternalOrderSnapshot =>
      allOrderWatchesState = allOrderWatchesState.applySnapshot(snapshot).orThrow

    case ItemAttachedStateEvent(key: InventoryItemKey, agentPath: AgentPath, attachedState: NotDetached) =>
      itemToAgentToAttachedState +=
        key -> (itemToAgentToAttachedState.getOrElse(key, Map.empty) + (agentPath -> attachedState))

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
    allOrderWatchesState = allOrderWatchesState.onEndOfRecovery.orThrow
  }

  protected def onAddEvent = {
    case Stamped(_, _, KeyedEvent(_: NoKey, ControllerEvent.ControllerInitialized(controllerId, startedAt))) =>
      controllerMetaState = controllerMetaState.copy(
        controllerId = controllerId,
        initiallyStartedAt = startedAt)

    case Stamped(_, _, KeyedEvent(_: NoKey, event: ControllerEvent.ControllerReady)) =>
      controllerMetaState = controllerMetaState.copy(
        timezone = event.timezone)

    case Stamped(_, _, KeyedEvent(_: NoKey, event: VersionedEvent)) =>
      repo = repo.applyEvent(event).orThrow

    case Stamped(_, _, KeyedEvent(_: NoKey, event: InventoryItemEvent)) =>
      event match {
        case event: UnsignedSimpleItemEvent =>
          event match {
            case UnsignedSimpleItemAdded(item) =>
              item match {
                case lock: Lock =>
                  _pathToLockState.insert(lock.path -> LockState(lock))

                case agentRef: AgentRef =>
                  _pathToAgentRefState.insert(agentRef.path -> AgentRefState(agentRef))

                case orderWatch: OrderWatch =>
                  allOrderWatchesState = allOrderWatchesState.addOrderWatch(orderWatch).orThrow

                case board: Board =>
                  _pathToBoardState.insert(board.path -> BoardState(board))

                case calendar: Calendar =>
                  _pathToCalendar.insert(calendar.path -> calendar)
              }

            case UnsignedSimpleItemChanged(item) =>
              item match {
                case lock: Lock =>
                  _pathToLockState(lock.path) = _pathToLockState(lock.path).copy(
                    lock = lock)

                case agentRef: AgentRef =>
                  _pathToAgentRefState(agentRef.path) = _pathToAgentRefState(agentRef.path).copy(
                    agentRef = agentRef)

                case orderWatch: OrderWatch =>
                  allOrderWatchesState = allOrderWatchesState.changeOrderWatch(orderWatch).orThrow

                case board: Board =>
                  _pathToBoardState += board.path ->
                    _pathToBoardState
                      .checked(board.path)
                      .map(boardState => boardState.copy(
                        board = board))
                      .orThrow

                case calendar: Calendar =>
                  _pathToCalendar += calendar.path -> calendar
              }
          }

        case event: SignedItemEvent =>
          event match {
            case event: SignedItemAdded =>
              onSignedItemAdded(event)

            case SignedItemChanged(Signed(item, signedString)) =>
              item match {
                case jobResource: JobResource =>
                  pathToSignedSimpleItem += jobResource.path -> Signed(jobResource, signedString)
              }
          }

        case event: BasicItemEvent.ForClient =>
          event match {
            case ItemAttachedStateEvent(itemKey, agentPath: AgentPath, attachedState) =>
              attachedState match {
                case attachedState: NotDetached =>
                  itemToAgentToAttachedState += itemKey ->
                    (itemToAgentToAttachedState.getOrElse(itemKey, Map.empty) +
                      (agentPath -> attachedState))

                case Detached =>
                  val updated = itemToAgentToAttachedState.getOrElse(itemKey, Map.empty) - agentPath
                  if (updated.isEmpty)
                    itemToAgentToAttachedState -= itemKey
                  else
                    itemToAgentToAttachedState += itemKey -> updated
              }

            case ItemDeletionMarked(itemKey) =>
              deletionMarkedItems += itemKey

            case ItemDeleted(itemKey) =>
              itemKey match {
                case id: VersionedItemId_ =>
                  repo = repo.deleteItem(id).orThrow

                case path: OrderWatchPath =>
                  deletionMarkedItems -= path
                  allOrderWatchesState = allOrderWatchesState.removeOrderWatch(path)

                case path: LockPath =>
                  _pathToLockState -= path

                case agentPath: AgentPath =>
                  _pathToAgentRefState -= agentPath

                case boardPath: BoardPath =>
                  _pathToBoardState -= boardPath

                case calendarPath: CalendarPath =>
                  _pathToCalendar -= calendarPath
              }
          }
      }

    case Stamped(_, _, KeyedEvent(name: AgentPath, event: AgentRefStateEvent)) =>
      _pathToAgentRefState += name -> _pathToAgentRefState(name).applyEvent(event).orThrow

    case Stamped(_, _, KeyedEvent(orderId: OrderId, event: OrderEvent)) =>
      event match {
        case orderAdded: OrderAdded =>
          addOrder(orderId, orderAdded)

        case orderAdded: OrderOrderAdded =>
          addOrder(orderAdded.orderId, orderAdded)
          _idToOrder(orderId) = _idToOrder(orderId).applyEvent(orderAdded).orThrow

        case orderDeleted: OrderDeleted =>
          for (order <- _idToOrder.remove(orderId)) {
            for (externalOrderKey <- order.externalOrderKey)
              allOrderWatchesState = allOrderWatchesState
                .onOrderEvent(externalOrderKey, orderId <-: orderDeleted)
                .orThrow
          }

        case event: OrderLockEvent =>
          for (lockPath <- event.lockPaths) {
            _pathToLockState(lockPath) = _pathToLockState(lockPath).applyEvent(orderId <-: event).orThrow
          }
          _idToOrder(orderId) = _idToOrder(orderId).applyEvent(event).orThrow

        case event: OrderNoticeEvent =>
          val boardState = orderIdToBoardState(orderId).orThrow
          val boardPath = boardState.path
          event match {
            case OrderNoticePosted(notice) =>
              _pathToBoardState += boardPath ->
                boardState.addNotice(notice).orThrow

            case OrderNoticeExpected(noticeId) =>
              pathToBoardState +=
                boardPath -> boardState.addExpectation(orderId, noticeId).orThrow

            case OrderNoticeRead =>
          }

          _idToOrder(orderId) = _idToOrder(orderId).applyEvent(event).orThrow

        case event: OrderCoreEvent =>
          val order = _idToOrder(orderId)
          handleForkJoinEvent(order, event)

          event match {
            case _: OrderDeletionMarked | _: OrderDeleted =>

            case _: OrderCancelled =>
              for (order <- order.ifState[ExpectingNotice]) {
                for {
                  boardState <- orderIdToBoardState(orderId)
                  updatedBoardState <- boardState.removeExpectation(orderId, order.state.noticeId)
                } {
                  _pathToBoardState(boardState.path) = updatedBoardState
                }
              }

            case _ =>
          }

          _idToOrder(orderId) = order.applyEvent(event).orThrow

          for (externalOrderKey <- order.externalOrderKey) {
            allOrderWatchesState = allOrderWatchesState
              .onOrderEvent(externalOrderKey, orderId <-: event)
              .orThrow
          }

        case _: OrderStdWritten =>
      }

    case Stamped(_, _, KeyedEvent(orderWatchPath: OrderWatchPath, event: OrderWatchEvent)) =>
      allOrderWatchesState = allOrderWatchesState.onOrderWatchEvent(orderWatchPath <-: event).orThrow

    case Stamped(_, _, KeyedEvent(boardPath: BoardPath, NoticePosted(notice))) =>
      for (boardState <- _pathToBoardState.get(boardPath)) {
        _pathToBoardState(boardState.path) = boardState.addNotice(notice).orThrow
      }

    case Stamped(_, _, KeyedEvent(boardPath: BoardPath, NoticeDeleted(noticeId))) =>
      for (boardState <- _pathToBoardState.get(boardPath)) {
        _pathToBoardState(boardState.path) = boardState.deleteNotice(noticeId).orThrow
      }

    case Stamped(_, _, KeyedEvent(_, _: ControllerShutDown)) =>
    case Stamped(_, _, KeyedEvent(_, ControllerTestEvent)) =>

    case Stamped(_, _, KeyedEvent(_: NoKey, event: JournalEvent)) =>
      standards = standards.copy(
        journalState = standards.journalState.applyEvent(event))

    case Stamped(_, _, KeyedEvent(_: NoKey, event: ClusterEvent)) =>
      standards = standards.copy(
        clusterState = standards.clusterState.applyEvent(event).orThrow)
  }

  private def addOrder(orderId: OrderId, orderAdded: OrderAddedX): Unit = {
    _idToOrder.insert(orderId -> Order.fromOrderAdded(orderId, orderAdded))
    allOrderWatchesState = allOrderWatchesState.onOrderAdded(orderId <-: orderAdded)
      .orThrow
  }

  private def onSignedItemAdded(added: SignedItemEvent.SignedItemAdded): Unit =
    added.signed.value match {
      case jobResource: JobResource =>
        pathToSignedSimpleItem.insert(jobResource.path -> Signed(jobResource, added.signedString))
    }

  private def handleForkJoinEvent(order: Order[Order.State], event: OrderCoreEvent): Unit =  // TODO Duplicate with Agent's OrderJournalRecoverer
    event match {
      case event: OrderForked =>
        for (childOrder <- order.newForkedOrders(event)) {
          _idToOrder += childOrder.id -> childOrder
        }

      case event: OrderJoined =>
        order.state match {
          case forked: Order.Forked =>
            _idToOrder --= forked.childOrderIds

          case state =>
            sys.error(s"Event $event recovered, but ${order.id} is in state $state")
        }

      case _ =>
    }

  def result() =
    ControllerState(
      eventId = eventId,
      standards,
      controllerMetaState,
      _pathToAgentRefState.toMap,
      _pathToLockState.toMap,
      _pathToBoardState.toMap,
      _pathToCalendar.toMap,
      allOrderWatchesState,
      repo,
      pathToSignedSimpleItem.toMap,
      itemToAgentToAttachedState.toMap,
      deletionMarkedItems.toSet,
      _idToOrder.toMap)

  def journalState = standards.journalState

  def clusterState = standards.clusterState
}
