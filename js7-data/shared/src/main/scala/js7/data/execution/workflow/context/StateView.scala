package js7.data.execution.workflow.context

import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.NotImplementedMap
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.controller.ControllerId
import js7.data.lock.{LockPath, LockState}
import js7.data.order.{Order, OrderId}
import js7.data.value.expression.Scope
import js7.data.value.expression.scopes.OrderScopes
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{End, Instructions}
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{Instruction, Workflow, WorkflowId}
import scala.reflect.ClassTag

trait StateView
{
  def isAgent: Boolean

  def controllerId: ControllerId

  def idToOrder: Map[OrderId, Order[Order.State]]

  def idToWorkflow: PartialFunction[WorkflowId, Workflow]

  def pathToLockState: Map[LockPath, LockState]

  def instruction(workflowPosition: WorkflowPosition): Instruction =
    idToWorkflow
      .checked(workflowPosition.workflowId)
      .map(_.instruction(workflowPosition.position))
      .orThrow

  final def instruction_[A <: Instruction: ClassTag](workflowPosition: WorkflowPosition)
  : Checked[Instruction] =
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

  def childOrderEnded(order: Order[Order.State]): Boolean =
    order.parent.flatMap(idToOrder.get) match {
      case Some(parentOrder) =>
        lazy val endReached = instruction(order.workflowPosition).isInstanceOf[End] &&
          order.state == Order.Ready &&
          order.position.dropChild.contains(parentOrder.position)
        order.attachedState == parentOrder.attachedState &&
          (endReached || order.isState[Order.FailedInFork])
      case _ => false
    }

  final def toScope(order: Order[Order.State]): Checked[Scope] =
    for (w <- idToWorkflow.checked(order.workflowId)) yield
      OrderScopes(order, w, controllerId).orderScope
}

object StateView
{
  def forTest(
    isAgent: Boolean,
    controllerId: ControllerId = ControllerId("CONTROLLER"),
    idToOrder: Map[OrderId, Order[Order.State]] = new NotImplementedMap,
    idToWorkflow: Map[WorkflowId, Workflow] = new NotImplementedMap,
    pathToLockState: Map[LockPath, LockState] = new NotImplementedMap
  ) = {
    val isAgent_ = isAgent
    val controllerId_ = controllerId
    val idToOrder_ = idToOrder
    val idToWorkflow_ = idToWorkflow
    val pathToLockState_ = pathToLockState

    new StateView {
      val isAgent = isAgent_
      val idToOrder = idToOrder_
      val idToWorkflow = idToWorkflow_
      val pathToLockState = pathToLockState_
      val controllerId = controllerId_
    }
  }

  trait ForTest extends StateView
  {
    def idToOrder: Map[OrderId, Order[Order.State]] =
      new NotImplementedMap[OrderId, Order[Order.State]]

    def idToWorkflow: Map[WorkflowId, Workflow] =
      new NotImplementedMap[WorkflowId, Workflow]

    def pathToLockState: Map[LockPath, LockState] =
      new NotImplementedMap[LockPath, LockState]

    val controllerId = ControllerId("CONTROLLER")
  }
}
