package js7.data.execution.workflow.context

import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.implicitClass
import js7.data.controller.ControllerId
import js7.data.lock.{LockPath, LockState}
import js7.data.order.{Order, OrderId}
import js7.data.value.expression.Scope
import js7.data.value.expression.scopes.OrderScopes
import js7.data.workflow.instructions.Instructions
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{Instruction, Workflow, WorkflowId}
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
trait StateView
{
  def idToOrder: OrderId => Checked[Order[Order.State]]

  def pathToLockState: LockPath => Checked[LockState]

  def controllerId: ControllerId

  def instruction(workflowPosition: WorkflowPosition): Instruction =
    idToWorkflow(workflowPosition.workflowId)
      .map(_.instruction(workflowPosition.position))
      .orThrow

  def instruction_[A <: Instruction: ClassTag](workflowPosition: WorkflowPosition): Checked[Instruction] =
    instruction(workflowPosition) match {
      case o if implicitClass[A] isAssignableFrom o.getClass =>
        Right(o.asInstanceOf[A])
      case o =>
        Left(Problem(s"An Instruction '${Instructions.jsonCodec.classToName(implicitClass[A])}' " +
          s"is expected at position $workflowPosition, not: ${Instructions.jsonCodec.typeName(o)}"))
    }

  final def workflowJob(workflowPosition: WorkflowPosition): Checked[WorkflowJob] =
    for {
      workflow <- idToWorkflow(workflowPosition.workflowId)
      job <- workflow.checkedWorkflowJob(workflowPosition.position)
    } yield job

  def childOrderEnded(order: Order[Order.State]): Boolean

  protected def idToWorkflow(id: WorkflowId): Checked[Workflow]

  final def toScope(order: Order[Order.State]): Checked[Scope] =
    for (w <- idToWorkflow(order.workflowId)) yield
      OrderScopes(order, w, controllerId).orderScope
}
