package js7.data.state

import cats.syntax.semigroup._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.NotImplementedMap
import js7.base.utils.ScalaUtils.syntax._
import js7.data.board.{BoardPath, BoardState}
import js7.data.controller.ControllerId
import js7.data.item.{InventoryItem, InventoryItemKey, SimpleItem, SimpleItemPath, UnsignedSimpleItem, UnsignedSimpleItemPath}
import js7.data.job.{JobKey, JobResource}
import js7.data.lock.{LockPath, LockState}
import js7.data.order.Order.{FailedInFork, Processing}
import js7.data.order.{Order, OrderId}
import js7.data.value.expression.Scope
import js7.data.value.expression.scopes.{JobResourceScope, NowScope, OrderScopes}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{BoardInstruction, End}
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{Instruction, Workflow, WorkflowId, WorkflowPath}
import scala.collection.MapView
import scala.reflect.ClassTag

trait StateView
{
  def isAgent: Boolean

  def controllerId: ControllerId

  def idToOrder: PartialFunction[OrderId, Order[Order.State]]

  def orders: Iterable[Order[Order.State]]

  // SLOW !!!
  def jobToOrderCount(jobKey: JobKey): Int =
    idToWorkflow
      .get(jobKey.workflowId)
      .fold(0)(workflow =>
        orders.view
          .count(order =>
            order.state.isInstanceOf[Processing] &&
              order.workflowId == jobKey.workflowId &&
              workflow.positionToJobKey(order.position).contains(jobKey)))

  def idToWorkflow: PartialFunction[WorkflowId, Workflow]

  def keyToItem: MapView[InventoryItemKey, InventoryItem]

  final lazy val pathToSimpleItem: MapView[SimpleItemPath, SimpleItem] =
    keyToItem.asInstanceOf[MapView[SimpleItemPath, SimpleItem]]
      .filter(_._2.isInstanceOf[SimpleItem])

  lazy val pathToUnsignedSimpleItem: MapView[UnsignedSimpleItemPath, UnsignedSimpleItem] =
    keyToItem
      .filter(_._2.isInstanceOf[UnsignedSimpleItem])
      .asInstanceOf[MapView[UnsignedSimpleItemPath, UnsignedSimpleItem]]

  final def keyTo[I <: InventoryItem](I: InventoryItem.Companion[I]): MapView[I.Key, I] =
    keyToItem
      .filter { case (_, v) => I.cls.isAssignableFrom(v.getClass) }
      .asInstanceOf[MapView[I.Key, I]]

  def workflowPathToId(workflowPath: WorkflowPath): Checked[WorkflowId]

  def pathToLockState: PartialFunction[LockPath, LockState]

  def pathToBoardState: PartialFunction[BoardPath, BoardState]

  final def workflowPositionToBoardState(workflowPosition: WorkflowPosition): Checked[BoardState] =
    for {
      boardPath <- workflowPositionToBoardPath(workflowPosition)
      boardState <- pathToBoardState.checked(boardPath)
    } yield boardState

  final def workflowPositionToBoardPath(workflowPosition: WorkflowPosition): Checked[BoardPath] =
    instruction_[BoardInstruction](workflowPosition)
      .map(_.boardPath)

  def instruction(workflowPosition: WorkflowPosition): Instruction =
    idToWorkflow
      .checked(workflowPosition.workflowId)
      .map(_.instruction(workflowPosition.position))
      .orThrow

  final def instruction_[A <: Instruction: ClassTag](workflowPosition: WorkflowPosition)
  : Checked[A] =
    idToWorkflow
      .checked(workflowPosition.workflowId)
      .map(_.instruction_[A](workflowPosition.position))
      .orThrow

  private def keyToJob(jobKey: JobKey): Checked[WorkflowJob] =
    idToWorkflow.checked(jobKey.workflowId)
      .flatMap(_.keyToJob.checked(jobKey))

  final def workflowJob(workflowPosition: WorkflowPosition): Checked[WorkflowJob] =
    for {
      workflow <- idToWorkflow.checked(workflowPosition.workflowId)
      job <- workflow.checkedWorkflowJob(workflowPosition.position)
    } yield job

  def childOrderEnded(order: Order[Order.State], parent: Order[Order.Forked]): Boolean = {
    lazy val endReached = order.state == Order.Ready &&
      order.position.parent.contains(parent.position) &&
      instruction(order.workflowPosition).isInstanceOf[End]
    (order.isAttached || order.isDetached) &&
      order.attachedState == parent.attachedState &&
      (order.state.eq(FailedInFork) || endReached)
  }

  /** A pure (stable, repeatable) Scope. */
  final def toPureScope(order: Order[Order.State]): Checked[Scope] =
    for (orderScopes <- toOrderScopes(order)) yield
      orderScopes.pureOrderScope

  /** An impure (unstable, non-repeatable) Scope. */
  final def toImpureOrderExecutingScope(order: Order[Order.State], now: Timestamp): Checked[Scope] =
    for (orderScopes <- toOrderScopes(order)) yield {
      val nowScope = NowScope(now)
      orderScopes.pureOrderScope |+| nowScope |+|
        JobResourceScope(keyTo(JobResource),
          useScope = orderScopes.variablelessOrderScope |+| nowScope)
    }

  final def toOrderScopes(order: Order[Order.State]): Checked[OrderScopes] =
    for (w <- idToWorkflow.checked(order.workflowId)) yield
      OrderScopes(order, w, controllerId)

  protected def orderIdToBoardState(orderId: OrderId)
  : Checked[BoardState] =
    for {
      order <- idToOrder.checked(orderId)
      instr <- instruction_[BoardInstruction](order.workflowPosition)
      boardState <- pathToBoardState.checked(instr.boardPath)
    } yield boardState
}

object StateView
{
  def forTest(
    isAgent: Boolean,
    controllerId: ControllerId = ControllerId("CONTROLLER"),
    idToOrder: Map[OrderId, Order[Order.State]] = new NotImplementedMap,
    idToWorkflow: PartialFunction[WorkflowId, Workflow] = new NotImplementedMap,
    pathToLockState: PartialFunction[LockPath, LockState] = new NotImplementedMap,
    pathToBoardState: PartialFunction[BoardPath, BoardState] = new NotImplementedMap)
  = {
    val isAgent_ = isAgent
    val controllerId_ = controllerId
    val idToOrder_ = idToOrder
    val idToWorkflow_ = idToWorkflow
    val pathToLockState_ = pathToLockState
    val pathToBoardState_ = pathToBoardState

    new StateView {
      val isAgent = isAgent_
      val idToOrder = idToOrder_
      val orders = idToOrder_.values
      val idToWorkflow = idToWorkflow_
      val pathToLockState = pathToLockState_
      val pathToBoardState = pathToBoardState_
      val controllerId = controllerId_

      def workflowPathToId(workflowPath: WorkflowPath) =
        Left(Problem("workflowPathToId is not implemented"))

      lazy val keyToItem: MapView[InventoryItemKey, InventoryItem] =
        new MapView[InventoryItemKey, InventoryItem] {
          def get(itemKey: InventoryItemKey): Option[InventoryItem] =
            itemKey match {
              case WorkflowId.as(id) => idToWorkflow.get(id)
              case path: LockPath => pathToLockState.get(path).map(_.item)
              case path: BoardPath => pathToBoardState.get(path).map(_.item)
            }

          def iterator: Iterator[(InventoryItemKey, InventoryItem)] =
            throw new NotImplementedError
        }
    }
  }

  trait ForTest extends StateView
  {
    def idToOrder: Map[OrderId, Order[Order.State]] =
      new NotImplementedMap[OrderId, Order[Order.State]]

    def orders: Iterable[Order[Order.State]] =
      idToOrder.values

    def idToWorkflow: PartialFunction[WorkflowId, Workflow] =
      new NotImplementedMap[WorkflowId, Workflow]

    def workflowPathToId(workflowPath: WorkflowPath) =
      Left(Problem("workflowPathToId is not implemented"))

    def pathToLockState: PartialFunction[LockPath, LockState] =
      new NotImplementedMap[LockPath, LockState]

    def pathToBoardState: PartialFunction[BoardPath, BoardState] =
      new NotImplementedMap[BoardPath, BoardState]

    val controllerId = ControllerId("CONTROLLER")
  }
}
