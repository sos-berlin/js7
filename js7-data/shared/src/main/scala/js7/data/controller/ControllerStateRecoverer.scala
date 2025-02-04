package js7.data.controller

import js7.base.crypt.Signed
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.utils.Collections.implicits.*
import js7.base.utils.Nulls.isNull
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.base.utils.{Nulls, StandardMapView}
import js7.data.agent.{AgentPath, AgentRefState}
import js7.data.board.{BoardPath, BoardState, NoticeSnapshot}
import js7.data.cluster.ClusterStateSnapshot
import js7.data.event.{JournalState, SnapshotableStateRecoverer, StandardsBuilder}
import js7.data.item.BasicItemEvent.{ItemAttachedStateEvent, ItemDeletionMarked}
import js7.data.item.SignedItemEvent.SignedItemAdded
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemAdded
import js7.data.item.{BasicItemEvent, ClientAttachments, InventoryItemKey, Repo, SignableSimpleItem, SignableSimpleItemPath, SignedItemEvent, UnsignedItemKey, UnsignedItemState, UnsignedSimpleItem, UnsignedSimpleItemEvent, UnsignedSimpleItemState, VersionedControl, VersionedEvent}
import js7.data.job.JobResource
import js7.data.order.{Order, OrderId}
import js7.data.orderwatch.{OrderWatch, OrderWatchPath, OrderWatchState, OrderWatchStateHandler}
import js7.data.plan.{PlanId, PlanSchema, PlanSchemaState}
import js7.data.state.WorkflowAndOrderRecovering.followUpRecoveredWorkflowsAndOrders
import js7.data.subagent.SubagentItemState
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import scala.collection.{MapView, mutable}

final class ControllerStateRecoverer
extends
  SnapshotableStateRecoverer[ControllerState],
  StandardsBuilder,
  ControllerStateView,
  OrderWatchStateHandler[ControllerStateRecoverer]:

  protected val S = ControllerState

  private var controllerMetaState = ControllerMetaState.Undefined
  private var repo = Repo.empty
  private val _idToOrder = mutable.Map.empty[OrderId, Order[Order.State]]
  private val _keyToUnsignedItemState: mutable.Map[UnsignedItemKey, UnsignedItemState] =
    ControllerState.empty.keyToUnsignedItemState_.to(mutable.Map)
  private var agentAttachments = ClientAttachments.empty[AgentPath]
  private val deletionMarkedItems = mutable.Set[InventoryItemKey]()
  private val pathToSignedSimpleItem = mutable.Map.empty[SignableSimpleItemPath, Signed[SignableSimpleItem]]
  private var _controllerState: ControllerState = null.asInstanceOf[ControllerState]

  protected def updateOrderWatchStates(
    orderWatchStates: Seq[OrderWatchState],
    remove: Seq[OrderWatchPath])
  : Checked[ControllerStateRecoverer] =
    _keyToUnsignedItemState --= remove
    _keyToUnsignedItemState ++= orderWatchStates.map(o => o.path -> o)
    Right(this)

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

  def checkedBoardState(path: BoardPath): Checked[BoardState] =
    _keyToUnsignedItemState.checked(path).asInstanceOf[Checked[BoardState]]

  def workflowPathToId(workflowPath: WorkflowPath): Checked[WorkflowId] =
    repo.pathToId(workflowPath)
      .toRight(UnknownKeyProblem("WorkflowPath", workflowPath.string))

  val keyToUnsignedItemState: MapView[UnsignedItemKey, UnsignedItemState] =
    _keyToUnsignedItemState.view

  protected def onInitializeState(state: ControllerState): Unit =
    assert(isNull(_controllerState))
    _controllerState = state

  protected def onAddSnapshotObject =
    case order: Order[Order.State] =>
      _idToOrder.insert(order.id, order)

      order.state match
        case Order.ExpectingNotice(noticeKey) =>
          // COMPATIBLE with v2.3
          val boardState =
            workflowPositionToBoardPath(order.workflowPosition).flatMap: boardPath =>
              checkedBoardState(boardPath)
            .orThrow
          val noticeId = PlanId.Global / boardState.path / noticeKey
          _keyToUnsignedItemState(boardState.path) =
            boardState.addExpectation(noticeId.plannedNoticeKey, order.id).orThrow

          // Change ExpectingNotice (Orders of v2.3) to ExpectingNotices
          _idToOrder.update(order.id, order.copy(
            state = Order.ExpectingNotices(Vector:
              PlanId.Global / boardState.path / noticeKey)))

        case Order.ExpectingNotices(noticeIds) =>
          _keyToUnsignedItemState ++=
            noticeIds.map: noticeId =>
              noticeId.boardPath ->
                checkedBoardState(noticeId.boardPath).flatMap: boardState =>
                  boardState.addExpectation(noticeId.plannedNoticeKey, order.id)
                .orThrow

        case _ =>

    case event: VersionedEvent =>
      repo = repo.applyEvent(event).orThrow

    case agentRefState: AgentRefState =>
      val (agentRef, maybeSubagentItem) = agentRefState.agentRef.convertFromV2_1.orThrow

      _keyToUnsignedItemState.insert(agentRef.path, agentRefState.copy(agentRef = agentRef))
      for subagentItem <- maybeSubagentItem do
        _keyToUnsignedItemState.insert(subagentItem.id, SubagentItemState.initial(subagentItem))

    case snapshot: NoticeSnapshot =>
      _keyToUnsignedItemState(snapshot.boardPath) =
        checkedBoardState(snapshot.boardPath).orThrow
          .recover(snapshot).orThrow

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

    case snapshot: PlanSchemaState.Snapshot =>
      val planSchemaState = _keyToUnsignedItemState(snapshot.id).asInstanceOf[PlanSchemaState]
      _keyToUnsignedItemState(snapshot.id) = planSchemaState.recover(snapshot)

    case o: ControllerMetaState =>
      controllerMetaState = o

    case o @ (_: JournalState | _: ClusterStateSnapshot) =>
      addStandardObject(o)

  override protected def onOnAllSnapshotsObjectsAdded(): Unit =
    assert(isNull(_controllerState))
    val (added, deleted) = followUpRecoveredWorkflowsAndOrders(repo.idTo(Workflow), _idToOrder.toMap)
    _idToOrder ++= added
    _idToOrder --= deleted
    ow.finishRecovery.orThrow
    _controllerState = ControllerState(
      eventId = eventId,
      _standards,
      controllerMetaState,
      _keyToUnsignedItemState.view.mapValues:
        case o: PlanSchemaState => o.copy(toPlan = Map.empty) // finish will recalculate properly
        case o => o
      .toMap,
      repo,
      pathToSignedSimpleItem.toMap,
      agentAttachments,
      deletionMarkedItems.toSet,
      _idToOrder.toMap
    ).finish.orThrow

    repo = Repo.empty
    _idToOrder.clear()
    _keyToUnsignedItemState.clear()
    agentAttachments = ClientAttachments.empty[AgentPath]
    deletionMarkedItems.clear()
    pathToSignedSimpleItem.clear()

  private def onSignedItemAdded(added: SignedItemEvent.SignedItemAdded): Unit =
    added.signed.value match
      case jobResource: JobResource =>
        pathToSignedSimpleItem.insert(jobResource.path, Signed(jobResource, added.signedString))

  protected def pathToOrderWatchState: MapView[OrderWatchPath, OrderWatchState] =
    new StandardMapView[OrderWatchPath, OrderWatchState]:
      override def keySet: Set[OrderWatchPath] =
        _keyToUnsignedItemState.keys.view.collect:
          case k: OrderWatchPath => k
        .toSet

      def get(path: OrderWatchPath): Option[OrderWatchState] =
        _keyToUnsignedItemState.get(path).asInstanceOf[Option[OrderWatchState]]

  def result(): ControllerState =
    _controllerState.nn.copy(eventId = eventId)
