package js7.data.state

import cats.syntax.traverse.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.CatsUtils.combine
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isStrict
import js7.data.Problems.EventNotHandledHereProblem
import js7.data.agent.AgentPath
import js7.data.board.BoardState
import js7.data.cluster.ClusterState
import js7.data.controller.ControllerId
import js7.data.event.{Event, EventDrivenState, EventDrivenState_, KeyedEvent, SignedItemContainer}
import js7.data.item.{InventoryItem, InventoryItemKey, InventoryItemState, UnsignedItemKey, UnsignedItemState, UnsignedSimpleItem, UnsignedSimpleItemPath, UnsignedSimpleItemState}
import js7.data.job.{JobKey, JobResource}
import js7.data.lock.{LockPath, LockState}
import js7.data.order.Order.{Cancelled, ExpectingNotices, FailedInFork, IsFreshOrReady, Processing, WaitingForLock}
import js7.data.order.OrderEvent.{LockDemand, OrderAddedX, OrderCancelled, OrderCoreEvent, OrderDeleted, OrderDeletionMarked, OrderDetached, OrderExternalVanished, OrderForked, OrderJoined, OrderLockEvent, OrderLocksAcquired, OrderLocksQueued, OrderLocksReleased, OrderOrderAdded, OrderStateReset, OrderStdWritten}
import js7.data.order.{MinimumOrder, Order, OrderEvent, OrderId}
import js7.data.plan.PlanSchemaState
import js7.data.state.EngineState.*
import js7.data.state.EngineStateStatistics
import js7.data.value.expression.Scope
import js7.data.value.expression.scopes.{JobResourceScope, NamedValueScope, NowScope, OrderScopes}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{End, Execute, LockInstruction, NoticeInstruction}
import js7.data.workflow.position.{Label, WorkflowPosition}
import js7.data.workflow.{Instruction, Workflow, WorkflowControl, WorkflowControlId, WorkflowId, WorkflowPath, WorkflowPathControl, WorkflowPathControlPath}
import scala.collection.MapView
import scala.reflect.ClassTag

/** Common interface for ControllerState and AgentState (but not SubagentState). */
trait EngineState extends EventDrivenState[Event], SignedItemContainer, EngineStateFunctions:

  type This <: EngineState_[This]

  def companion: Companion[This]

  def isAgent: Boolean

  def clusterState: ClusterState

  def maybeAgentPath: Option[AgentPath] = None

  def controllerId: ControllerId

  def idToOrder: Map[OrderId, Order[Order.State]]

  def orders: Iterable[Order[Order.State]]

  def statistics: EngineStateStatistics

  final def weHave(order: Order[Order.State]) =
    order.isDetached && !isAgent ||
      order.isAttached && isAgent

  // TODO SLOW!
  final def slowProcessingOrderCount(agentPath: AgentPath): Int =
    orders.iterator.filter(_.attached.contains(agentPath)).count(_.isState[Processing])

  // SLOW !!!
  final def jobToOrderCount(jobKey: JobKey): Int =
    idToWorkflow
      .get(jobKey.workflowId)
      .fold(0)(workflow =>
        orders.view
          .count(order =>
            order.state.isInstanceOf[Processing] &&
              order.workflowId == jobKey.workflowId &&
              workflow.positionToJobKey(order.position).contains(jobKey)))

  def idToWorkflow: PartialFunction[WorkflowId, Workflow]

  def workflowPathToId(workflowPath: WorkflowPath): Checked[WorkflowId]

  final def pathToWorkflowPathControl: MapView[WorkflowPathControlPath, WorkflowPathControl] =
    keyTo(WorkflowPathControl).mapValues(_.item)

  def keyToUnsignedItemState: MapView[UnsignedItemKey, UnsignedItemState]

  final def keyTo[A <: InventoryItemState](A: InventoryItemState.Companion[A]): MapView[A.Key, A] =
    keyToUnsignedItemState
      .filter((_, v) => v.companion eq A)
      .asInstanceOf[MapView[A.Key, A]]

  final def pathToUnsignedSimple[A <: UnsignedSimpleItem](A: UnsignedSimpleItem.Companion[A])
  : MapView[A.Path, A] =
    keyToUnsignedItemState
      .filter((_, v) => v.item.companion eq A)
      .mapValues(_.item)
      .asInstanceOf[MapView[A.Path, A]]

  def checkOrdersDoNotExist(orderIds: Iterable[OrderId]): Checked[Unit] =
    if isStrict then
      val known = orderIds.filter(idToOrder.isDefinedAt)
      known.isEmpty !! Problem(s"Orders already exist: ${known.toArray.sorted.mkString(", ")}")
    else
      Checked.unit

  def checkOrdersExist(orderIds: Iterable[OrderId]): Checked[Unit] =
    if isStrict then
      val unknown = orderIds.filterNot(idToOrder.isDefinedAt)
      unknown.isEmpty !! Problem(s"Unknown Orders: ${unknown.toArray.sorted.mkString(", ")}")
    else
      Checked.unit

  def isOrderProcessable(orderId: OrderId): Boolean =
    idToOrder.get(orderId).exists(isOrderProcessable)

  def isOrderProcessable(order: Order[Order.State]): Boolean =
    order.isProcessable && isOrderProcessable2(order)

  def ifOrderProcessable(order: Order[Order.State]): Option[Order[IsFreshOrReady]] =
    order.ifProcessable.flatMap: order =>
      isOrderProcessable2(order) ? order

  private def isOrderProcessable2(order: Order[Order.State]): Boolean =
    instruction(order.workflowPosition).isInstanceOf[Execute]
      && order.isAttached
      && !isOrderAtStopPosition(order)
      && !isOrderAtBreakpoint(order)
      && !isWorkflowSuspended(order.workflowPath)

  def isOrderAtStopPosition(order: Order[Order.State]): Boolean =
    order.stopPositions.nonEmpty/*return fast*/ &&
      idToWorkflow(order.workflowId).isOrderAtStopPosition(order)

  def isOrderAtBreakpoint(order: Order[Order.State]): Boolean =
    order.isState[Order.IsFreshOrReady] &&
      order.isSuspendibleNow && !order.isResumed &&
      keyTo(WorkflowControl)
        .get(WorkflowControlId(order.workflowId))
        .exists(_.breakpoints contains order.position.normalized)

  final def workflowJob(workflowPosition: WorkflowPosition): Checked[WorkflowJob] =
    for
      workflow <- idToWorkflow.checked(workflowPosition.workflowId)
      job <- workflow.checkedWorkflowJob(workflowPosition.position)
    yield
      job

  private def keyToJob(jobKey: JobKey): Checked[WorkflowJob] =
    idToWorkflow.checked(jobKey.workflowId)
      .flatMap(_.keyToJob.checked(jobKey))

  // COMPATIBLE with v2.3
  protected def orderIdToBoardState(orderId: OrderId): Checked[BoardState] =
    for
      order <- idToOrder.checked(orderId)
      instr <- instruction_[NoticeInstruction](order.workflowPosition)
      boardPath <- instr.referencedBoardPaths.toVector match
        case Vector(o) => Right(o)
        case _ => Left(Problem.pure("Legacy orderIdToBoardState, but instruction has multiple BoardPaths"))
      boardState <- keyTo(BoardState).checked(boardPath)
    yield
      boardState

  final def workflowPositionToLabel(workflowPosition: WorkflowPosition): Checked[Option[Label]] =
    for
      workflow <- idToWorkflow.checked(workflowPosition.workflowId)
      labeled <- workflow.labeledInstruction(workflowPosition.position)
    yield
      labeled.maybeLabel

  def childOrderIsJoinable(order: Order[Order.State], parent: Order[Order.Forked]): Boolean =
    !order.isSuspendedOrStopped &&
    order.isDetachedOrAttached &&
      order.attachedState == parent.attachedState &&
      (order.state.eq(FailedInFork) ||
        order.state.eq(Cancelled) ||
        order.isState[Order.Ready] &&
          order.position.parent.contains(parent.position) &&
          instruction(order.workflowPosition).isInstanceOf[End])

  final def isSuspendedOrStopped(order: Order[Order.State]): Boolean =
    order.isSuspendedOrStopped || isWorkflowSuspended(order.workflowPath)

  final def isWorkflowSuspended(workflowPath: WorkflowPath): Boolean =
    keyTo(WorkflowPathControl)
      .get(WorkflowPathControlPath(workflowPath))
      .exists(_.item.suspended)

  /** Scope to calculate PlanId and NoticeKey of a PlannableBoard.  */
  def toPlanOrderScope(order: MinimumOrder): Scope =
    // TODO Add final Workflow defined named values ?
    OrderScopes.minimumOrderScope(order.id, order, controllerId)
      |+| NamedValueScope(order.arguments)

  /** A pure (stable, repeatable) Scope. */
  final def toOrderScope(order: Order[Order.State]): Checked[Scope] =
    toOrderScopes(order).map(_.pureOrderScope)

  /** An impure (unstable, non-repeatable) Scope. */
  final def toImpureOrderExecutingScope(order: Order[Order.State], now: Timestamp): Checked[Scope] =
    for orderScopes <- toOrderScopes(order) yield
      val nowScope = NowScope(now)
      combine(
        orderScopes.pureOrderScope,
        nowScope,
        JobResourceScope(
          keyTo(JobResource),
          useScope = orderScopes.variablelessOrderScope |+| nowScope))

  final def noticeScope(order: Order[Order.State]): Checked[Scope] =
    toOrderScopes(order).map(_.pureOrderScope)

  final def toOrderScopes(order: Order[Order.State]): Checked[OrderScopes] =
    idToWorkflow.checked(order.workflowId).map:
      OrderScopes(order, _, controllerId)

  final def foreachLockDemand[A](demands: Seq[LockDemand])(op: (LockState, Option[Int]) => Checked[A])
  : Checked[Seq[A]] =
    demands.traverse: demand =>
      keyTo(LockState).checked(demand.lockPath)
        .flatMap(op(_, demand.count))

  final def foreachLock[A](lockPaths: Seq[LockPath])(op: LockState => Checked[A])
  : Checked[Seq[A]] =
    lockPaths.traverse: lockPath =>
      keyTo(LockState).checked(lockPath)
        .flatMap(op)

  protected def addOrders(orders: Seq[Order[Order.State]] = Nil, allowClosedPlan: Boolean)
  : Checked[This]

  protected final def update(
    updateOrders: Seq[Order[Order.State]] = Nil,
    removeOrders: Seq[OrderId] = Nil,
    externalVanishedOrders: Seq[Order[Order.State]] = Nil,
    addItemStates: Seq[UnsignedSimpleItemState] = Nil,
    removeUnsignedSimpleItems: Seq[UnsignedSimpleItemPath] = Nil)
  : Checked[This] =
    for
      _ <- precheckUpdate(updateOrders, removeOrders, externalVanishedOrders,
        addItemStates, removeUnsignedSimpleItems)
      self <- update_(updateOrders, removeOrders, externalVanishedOrders,
        addItemStates, removeUnsignedSimpleItems)
    yield
      self

  // Call only via update() !
  protected def update_(
    updateOrders: Seq[Order[Order.State]] = Nil,
    removeOrders: Seq[OrderId] = Nil,
    externalVanishedOrders: Seq[Order[Order.State]] = Nil,
    addItemStates: Seq[UnsignedSimpleItemState] = Nil,
    removeUnsignedSimpleItems: Seq[UnsignedSimpleItemPath] = Nil)
  : Checked[This]

  private def precheckUpdate(
    updatedOrders: Seq[Order[Order.State]] = Nil,
    removeOrders: Seq[OrderId] = Nil,
    externalVanishedOrders: Seq[Order[Order.State]] = Nil,
    addItemStates: Seq[UnsignedSimpleItemState] = Nil,
    removeUnsignedSimpleItems: Seq[UnsignedSimpleItemPath] = Nil)
  : Checked[Unit] =
    if isStrict then
      for
        _ <- updatedOrders.view.map(_.id).concat(removeOrders).checkUniqueness
        _ <- externalVanishedOrders.checkUniquenessBy(_.id)
        _ <- addItemStates.view.map(_.path).concat(removeUnsignedSimpleItems).checkUniqueness
      yield ()
    else
      Checked.unit

  protected def applyOrderEvent(orderId: OrderId, event: OrderEvent): Checked[This]

  protected def applyOrderCoreEvent(orderId: OrderId, event: OrderCoreEvent): Checked[This]

  protected final def findInstructionInCallStack[I <: Instruction : ClassTag](
    workflowPosition: WorkflowPosition)
  : Checked[I] =
    for
      pos <- workflowPosition.checkedParent
      instr <- instruction_[I](pos).orElse(findInstructionInCallStack[I](pos))
    yield
      instr

  protected final def checkChangedItem(item: UnsignedSimpleItem): Checked[Unit] =
    Checked.unit

  protected def removeNoticeExpectation(order: Order[ExpectingNotices])
  : Checked[Seq[PlanSchemaState]] =
    Left(Problem(s"removeNoticeExpectation not implemented for $companion"))


object EngineState:

  val empty: EngineState = NoEngineState()

  trait Companion[T <: EngineState_[T]]
    extends EventDrivenState.Companion[T]:
    override def updateStaticReference(engineState: T): Unit =
      EngineStateMXBean.setEngineState(engineState)


  private final case class NoEngineState(
    isAgent: Boolean = false,
    controllerId: ControllerId = ControllerId("UNKNOWN"),
    idToOrder: Map[OrderId, Order[Order.State]] = Map.empty,
    orders: Iterable[Order[Order.State]] = Nil,
    statistics: EngineStateStatistics = EngineStateStatistics.empty,
    idToWorkflow: PartialFunction[WorkflowId, Workflow] = Map.empty,
    keyToUnsignedItemState: MapView[UnsignedItemKey, UnsignedItemState] = MapView.empty,
    keyToItem: MapView[InventoryItemKey, InventoryItem] = MapView.empty)
  extends EngineState_[NoEngineState]:

    def companion = throw NotImplementedError()
    def clusterState: ClusterState = ClusterState.Empty

    def workflowPathToId(workflowPath: WorkflowPath): Left[Problem, Nothing] =
      reject

    protected def addOrders(orders: Seq[Order[Order.State]], allowClosedPlan: Boolean)
    : Left[Problem, Nothing] =
      reject

    protected def update_(
      updateOrders: Seq[Order[Order.State]],
      removeOrders: Seq[OrderId],
      externalVanishedOrders: Seq[Order[Order.State]],
      addItemStates: Seq[UnsignedSimpleItemState],
      removeUnsignedSimpleItems: Seq[UnsignedSimpleItemPath])
    : Left[Problem, Nothing] =
      reject

    def applyKeyedEvent(keyedEvent: KeyedEvent[Event]) =
      reject

    private def reject = Left(Problem.pure("NoEngineState"))


trait EngineState_[T <: EngineState_[T]] extends EngineState, EventDrivenState_[T, Event]:
  this: T =>
  type This = T

  override def companion: Companion[This]

  protected def applyOrderEvent(orderId: OrderId, event: OrderEvent): Checked[This] =
    event match
      case _: OrderAddedX | _: OrderEvent.OrderAttachedToAgent =>
        // Event is handled by one of ControllerState and AgentState only
        Left(EventNotHandledHereProblem(event, companion))

      case event: OrderCoreEvent =>
        applyOrderCoreEvent(orderId, event)

      case _: OrderStdWritten =>
        // OrderStdWritten is not applied. But check OrderId.
        idToOrder.checked(orderId).rightAs(this)

  protected final def applyOrderCoreEvent(orderId: OrderId, event: OrderCoreEvent): Checked[This] =
    for
      previousOrder <- idToOrder.checked(orderId)
      updatedOrder <- previousOrder.applyEvent(event)
      result <- event match
        case OrderDetached =>
          if isAgent then
            update(removeOrders = orderId :: Nil)
          else
            update(updateOrders = updatedOrder :: Nil)

        case event: OrderForked =>
          update(updatedOrder :: Nil).flatMap:
            _.addOrders(previousOrder.newForkedOrders(event), allowClosedPlan = true)

        case event: OrderJoined =>
          if isAgent then
            eventNotApplicable(orderId <-: event)
          else
            previousOrder.state match
              case forked: Order.Forked =>
                update(
                  updateOrders = updatedOrder :: Nil,
                  removeOrders = forked.childOrderIds)

              case state =>
                Left(Problem:
                  s"For event $event, $orderId must be in Forked state, not: $state")

        case event: OrderLockEvent =>
          event
            .match
              case OrderLocksQueued(demands) =>
                foreachLockDemand(demands):
                  _.enqueue(orderId, _)

              case OrderLocksAcquired(demands) =>
                foreachLockDemand(demands):
                  _.acquire(orderId, _)

              case OrderLocksReleased(lockPaths) =>
                foreachLock(lockPaths):
                  _.release(orderId)
            .flatMap: lockStates =>
              update(
                updateOrders = updatedOrder :: Nil,
                addItemStates = lockStates)

        case OrderStateReset =>
          previousOrder.ifState[WaitingForLock].map: order =>
            val instr = instruction_[LockInstruction](order.workflowPosition).orThrow
            foreachLock(instr.lockPaths): lockState =>
              Right:
                lockState.dequeue(orderId)
            .flatMap: lockStates =>
              update(
                updateOrders = updatedOrder :: Nil,
                addItemStates = lockStates)
          .orElse:
            previousOrder.ifState[ExpectingNotices].map: order =>
              removeNoticeExpectation(order).flatMap: updatedBoardStates =>
                update(
                  updateOrders = updatedOrder :: Nil,
                  addItemStates = updatedBoardStates)
          .getOrElse:
            update(
              updateOrders = updatedOrder :: Nil)

        case _: OrderCancelled =>
          previousOrder
            // COMPATIBLE Since v2.7.2 an OrderStateReset is emitted and the
            // following code is superfluous (but still needed for old journals)
            .ifState[ExpectingNotices]
            .fold(update(updateOrders = updatedOrder :: Nil)): order =>
              removeNoticeExpectation(order).flatMap: updatedBoardStates =>
                update(
                  updateOrders = updatedOrder :: Nil,
                  addItemStates = updatedBoardStates)

        case OrderExternalVanished =>
          if updatedOrder.externalOrder.isEmpty then
            Left(Problem(s"OrderExternalVanished but $orderId is not linked to an external order"))
          else
            update(externalVanishedOrders = updatedOrder :: Nil)

        case OrderDeletionMarked =>
          update(updateOrders = updatedOrder :: Nil)

        case OrderDeleted =>
          if isAgent then
            eventNotApplicable(orderId <-: event)
          else
            update(removeOrders = orderId :: Nil)

        case event: OrderOrderAdded =>
          // ControllerState handles this event
          Left(EventNotHandledHereProblem(event, companion))

        case _ =>
          update(updateOrders = updatedOrder :: Nil)
    yield
      result
