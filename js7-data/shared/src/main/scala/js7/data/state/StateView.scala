package js7.data.state

import cats.syntax.semigroup.*
import cats.syntax.traverse.*
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.L3
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.board.{BoardNoticeKey, BoardPath, BoardState, NoticeId}
import js7.data.controller.ControllerId
import js7.data.event.ItemContainer
import js7.data.item.{InventoryItemState, UnsignedItemKey, UnsignedItemState, UnsignedSimpleItem}
import js7.data.job.{JobKey, JobResource}
import js7.data.lock.{LockPath, LockState}
import js7.data.order.Order.{FailedInFork, IsFreshOrReady, Processing}
import js7.data.order.OrderEvent.LockDemand
import js7.data.order.{MinimumOrder, Order, OrderId}
import js7.data.plan.{PlanId, PlanSchemaId}
import js7.data.value.expression.Scope
import js7.data.value.expression.scopes.{JobResourceScope, NamedValueScope, NowScope, OrderScopes}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{End, NoticeInstruction}
import js7.data.workflow.position.{Label, WorkflowPosition}
import js7.data.workflow.{Instruction, Workflow, WorkflowControl, WorkflowControlId, WorkflowId, WorkflowPath, WorkflowPathControl, WorkflowPathControlPath}
import scala.collection.{MapView, View}
import scala.reflect.ClassTag

/** Common interface for ControllerState and AgentState (but not SubagentState). */
trait StateView extends ItemContainer:

  def isAgent: Boolean

  def maybeAgentPath: Option[AgentPath] = None

  def controllerId: ControllerId

  def idToOrder: PartialFunction[OrderId, Order[Order.State]]

  def orders: Iterable[Order[Order.State]]

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

  /** @return L3.True: Notice exists<br>
    *         L3.False: Notice doesn't exist but is announced<br>
    *         L3.Unknown: Notice doesn't exist nor is it announced
    */
  final def isNoticeAvailable(noticeId: NoticeId): L3 =
    keyTo(BoardState).get(noticeId.boardPath)
      .fold(L3.Unknown/*just in case*/): boardState =>
        boardState.isNoticeAvailable(noticeId.plannedNoticeKey)

  final def availableNotices(planId: PlanId, boardNoticeKeys: Iterable[BoardNoticeKey])
  : Set[BoardPath] =
    boardNoticeKeys.collect:
      case o if keyTo(BoardState).get(o.boardPath).exists(_.hasNotice(planId / o.noticeKey)) =>
        o.boardPath
    .toSet

  // COMPATIBLE with v2.3
  final def workflowPositionToBoardState(workflowPosition: WorkflowPosition): Checked[BoardState] =
    for
      boardPath <- workflowPositionToBoardPath(workflowPosition)
      boardState <- keyTo(BoardState).checked(boardPath)
    yield
      boardState

  // COMPATIBLE with v2.3
  final def workflowPositionToBoardPath(workflowPosition: WorkflowPosition): Checked[BoardPath] =
    instruction_[NoticeInstruction](workflowPosition).flatMap:
      _.referencedBoardPaths.toSeq match
        case Seq(boardPath) => Right(boardPath)
        case _ => Left(Problem.pure:
          "Legacy orderIdToBoardState, but instruction has multiple BoardPaths")

  final def removeNoticeKeysForPlanSchema(planSchemaId: PlanSchemaId)
  : View[(BoardPath, BoardState)] =
    keyTo(BoardState).values.view.flatMap: boardState =>
      boardState.removeNoticeKeysForPlanSchema(planSchemaId)
        .map(o => o.path -> o)

  def isOrderProcessable(order: Order[Order.State]): Boolean =
    order.isProcessable &&
      !isOrderAtStopPosition(order) &&
      !isOrderAtBreakpoint(order) &&
      !isWorkflowSuspended(order.workflowPath)

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

  def instruction(workflowPosition: WorkflowPosition): Instruction =
    idToWorkflow
      .checked(workflowPosition.workflowId)
      .orThrow
      .instruction(workflowPosition.position)

  final def instruction_[A <: Instruction: ClassTag](workflowPosition: WorkflowPosition)
  : Checked[A] =
    idToWorkflow
      .checked(workflowPosition.workflowId)
      .orThrow
      .instruction_[A](workflowPosition.position)

  final def workflowPositionToLabel(workflowPosition: WorkflowPosition): Checked[Option[Label]] =
    for
      workflow <- idToWorkflow.checked(workflowPosition.workflowId)
      labeled <- workflow.labeledInstruction(workflowPosition.position)
    yield
      labeled.maybeLabel

  def childOrderEnded(order: Order[Order.State], parent: Order[Order.Forked]): Boolean =
    lazy val endReached = order.state == Order.Ready &&
      order.position.parent.contains(parent.position) &&
      instruction(order.workflowPosition).isInstanceOf[End]
    order.isDetachedOrAttached &&
      order.attachedState == parent.attachedState &&
      (order.state.eq(FailedInFork) || endReached)

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

  ///** Scope to calculate PlanId and NoticeKey of a PlannableBoard.  */
  //private def toPlanOrderScopeXX(order: MinimumOrder): Checked[Scope] =
  //  keyToItem(Workflow).checked(order.workflowId).flatMap: workflow =>
  //    workflow.orderParameterList
  //    // TODO Add final Workflow defined named values ?
  //    OrderScopes.minimumOrderScope(order.id, order, controllerId)
  //      |+| NamedValueScope(arguments)

  /** A pure (stable, repeatable) Scope. */
  final def toOrderScope(order: Order[Order.State]): Checked[Scope] =
    toOrderScopes(order).map(_.pureOrderScope)

  /** An impure (unstable, non-repeatable) Scope. */
  final def toImpureOrderExecutingScope(order: Order[Order.State], now: Timestamp): Checked[Scope] =
    for orderScopes <- toOrderScopes(order) yield
      val nowScope = NowScope(now)
      orderScopes.pureOrderScope |+|
        nowScope |+|
        JobResourceScope(
          keyTo(JobResource),
          useScope = orderScopes.variablelessOrderScope |+| nowScope)

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
