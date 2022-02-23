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
import js7.data.item.SignedItemEvent.{SignedItemAdded, SignedItemChanged}
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemChanged}
import js7.data.item.{BasicItemEvent, ClientAttachments, InventoryItemEvent, InventoryItemKey, Repo, SignableSimpleItem, SignableSimpleItemPath, SignedItemEvent, UnsignedSimpleItemEvent, VersionedEvent, VersionedItemId_}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.order.Order.ExpectingNotice
import js7.data.order.OrderEvent.{OrderAdded, OrderAddedX, OrderCancelled, OrderCoreEvent, OrderDeleted, OrderForked, OrderJoined, OrderLockEvent, OrderNoticeEvent, OrderNoticeExpected, OrderNoticePosted, OrderNoticeRead, OrderOrderAdded, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{AllOrderWatchesState, OrderWatch, OrderWatchEvent, OrderWatchPath, OrderWatchState}
import js7.data.state.StateView
import js7.data.state.WorkflowAndOrderRecovering.followUpRecoveredWorkflowsAndOrders
import js7.data.subagent.{SubagentId, SubagentRef, SubagentRefState, SubagentRefStateEvent}
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import scala.collection.mutable

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
  private val _idToSubagentRefState = mutable.Map.empty[SubagentId, SubagentRefState]
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
    _idToSubagentRefState ++= state.idToSubagentRefState
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
      val (agentRef, subagentRef) = agentRefState.agentRef.convertFromLegacy.orThrow

      _pathToAgentRefState.insert(agentRef.path -> agentRefState.copy(agentRef = agentRef))
      for (subagentRef <- subagentRef) {
        _idToSubagentRefState.insert(subagentRef.id -> SubagentRefState.initial(subagentRef))
      }

    case subagentRefState: SubagentRefState =>
      _idToSubagentRefState.insert(subagentRefState.subagentId -> subagentRefState)

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

                case addedAgentRef: AgentRef =>
                  val (agentRef, subagentRef) = addedAgentRef.convertFromLegacy.orThrow

                  _pathToAgentRefState.insert(agentRef.path -> AgentRefState(agentRef))
                  for (subagentRef <- subagentRef) {
                    _idToSubagentRefState.insert(subagentRef.id -> SubagentRefState.initial(subagentRef))
                  }

                case agentRef: AgentRef =>
                  _pathToAgentRefState.insert(agentRef.path -> AgentRefState(agentRef))

                case subagentRef: SubagentRef =>
                  _idToSubagentRefState.insert(subagentRef.id -> SubagentRefState.initial(subagentRef))

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

                case changedAgentRef: AgentRef =>
                  val (agentRef, subagentRef) = changedAgentRef.convertFromLegacy.orThrow

                  _pathToAgentRefState(agentRef.path) = _pathToAgentRefState(agentRef.path).copy(
                    agentRef = agentRef)

                  for (subagentRef <- subagentRef) {
                    _idToSubagentRefState.updateWith(subagentRef.id) {
                      case None =>
                        Some(SubagentRefState.initial(subagentRef))

                      case Some(previous) =>
                        Some(previous.copy(
                          subagentRef = previous.subagentRef.copy(
                            uri = subagentRef.uri)))
                    }
                  }

                case subagentRef: SubagentRef =>
                  _idToSubagentRefState(subagentRef.id) = _idToSubagentRefState(subagentRef.id).copy(
                    subagentRef = subagentRef)

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
                  _idToSubagentRefState -= subagentId

                case boardPath: BoardPath =>
                  _pathToBoardState -= boardPath

                case calendarPath: CalendarPath =>
                  _pathToCalendar -= calendarPath

                case jobResourcePath: JobResourcePath =>
                  pathToSignedSimpleItem -= jobResourcePath
              }
          }
      }

    case Stamped(_, _, KeyedEvent(path: AgentPath, event: AgentRefStateEvent)) =>
      _pathToAgentRefState += path -> _pathToAgentRefState(path).applyEvent(event).orThrow

    case Stamped(_, _, KeyedEvent(id: SubagentId, event: SubagentRefStateEvent)) =>
      _idToSubagentRefState += id -> _idToSubagentRefState(id).applyEvent(event).orThrow

    case Stamped(_, _, KeyedEvent(orderId: OrderId, event: OrderEvent)) =>
      event match {
        case orderAdded: OrderAdded =>
          addOrder(orderId, orderAdded)

        case orderAdded: OrderOrderAdded =>
          addOrder(orderAdded.orderId, orderAdded)
          _idToOrder(orderId) = _idToOrder(orderId).applyEvent(orderAdded).orThrow

        case OrderDeleted =>
          for (order <- _idToOrder.remove(orderId)) {
            for (externalOrderKey <- order.externalOrderKey)
              allOrderWatchesState = allOrderWatchesState
                .onOrderDeleted(externalOrderKey, orderId)
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
            case _: OrderCancelled =>
              for (order <- order.ifState[ExpectingNotice]) {
                for {
                  boardState <- orderIdToBoardState(orderId)
                  updatedBoardState <- boardState.removeExpectation(orderId, order.state.noticeId)
                } {
                  _pathToBoardState(boardState.path) = updatedBoardState
                }
              }

            case _: OrderDeleted =>
              for (externalOrderKey <- order.externalOrderKey) {
                allOrderWatchesState = allOrderWatchesState
                  .onOrderDeleted(externalOrderKey, orderId)
                  .orThrow
              }

            case _ =>
          }

          _idToOrder(orderId) = order.applyEvent(event).orThrow

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
      _idToSubagentRefState.toMap,
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
