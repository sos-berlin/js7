package js7.data.execution.workflow.context

import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.NotImplementedMap
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.board.{Board, BoardPath, BoardState}
import js7.data.controller.ControllerId
import js7.data.lock.{LockPath, LockState}
import js7.data.order.{Order, OrderId}
import js7.data.value.expression.Scope
import js7.data.value.expression.scopes.OrderScopes
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{BoardInstruction, End, Instructions}
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{Instruction, Workflow, WorkflowId}
import scala.reflect.ClassTag

trait StateView
{
  def isAgent: Boolean

  def controllerId: ControllerId

  def idToOrder: PartialFunction[OrderId, Order[Order.State]]

  def idToWorkflow: PartialFunction[WorkflowId, Workflow]

  def pathToLockState: PartialFunction[LockPath, LockState]

  final def pathToBoard: PartialFunction[BoardPath, Board] =
    pathToBoardState.map(_.board)

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
    instruction(workflowPosition) match {
      case o if implicitClass[A] isAssignableFrom o.getClass =>
        Right(o.asInstanceOf[A])
      case o =>
        Left(Problem(s"An Instruction '${Instructions.jsonCodec.classToName(implicitClass[A])}' " +
          s"is expected at position $workflowPosition, not: ${Instructions.jsonCodec.typeName(o)}"))
    }

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
      (order.state.eq(Order.FailedInFork) || endReached)
  }

  final def toScope(order: Order[Order.State]): Checked[Scope] =
    for (w <- idToWorkflow.checked(order.workflowId)) yield
      OrderScopes(order, w, controllerId).orderScope

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
    idToOrder: PartialFunction[OrderId, Order[Order.State]] = new NotImplementedMap,
    idToWorkflow: PartialFunction[WorkflowId, Workflow] = new NotImplementedMap,
    pathToLockState: PartialFunction[LockPath, LockState] = new NotImplementedMap,
    pathToBoardState: PartialFunction[BoardPath, BoardState] = new NotImplementedMap
  ) = {
    val isAgent_ = isAgent
    val controllerId_ = controllerId
    val idToOrder_ = idToOrder
    val idToWorkflow_ = idToWorkflow
    val pathToLockState_ = pathToLockState
    val pathToBoardState_ = pathToBoardState

    new StateView {
      val isAgent = isAgent_
      val idToOrder = idToOrder_
      val idToWorkflow = idToWorkflow_
      val pathToLockState = pathToLockState_
      val pathToBoardState = pathToBoardState_
      val controllerId = controllerId_
    }
  }

  trait ForTest extends StateView
  {
    def idToOrder: PartialFunction[OrderId, Order[Order.State]] =
      new NotImplementedMap[OrderId, Order[Order.State]]

    def idToWorkflow: PartialFunction[WorkflowId, Workflow] =
      new NotImplementedMap[WorkflowId, Workflow]

    def pathToLockState: PartialFunction[LockPath, LockState] =
      new NotImplementedMap[LockPath, LockState]

    def pathToBoardState: PartialFunction[BoardPath, BoardState] =
      new NotImplementedMap[BoardPath, BoardState]

    val controllerId = ControllerId("CONTROLLER")
  }
}
