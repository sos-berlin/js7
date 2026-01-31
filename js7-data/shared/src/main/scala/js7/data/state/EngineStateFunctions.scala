package js7.data.state

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.order.{Order, OrderId}
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{Instruction, Workflow, WorkflowId}
import scala.reflect.ClassTag

// TODO Move functions to EngineStateExtensions and dissolve this trait
trait EngineStateFunctions:

  protected def idToWorkflow: PartialFunction[WorkflowId, Workflow]
  protected def idToOrder: PartialFunction[OrderId, Order[Order.State]]

  def instruction_[A <: Instruction : ClassTag](workflowPosition: WorkflowPosition)
  : Checked[A] =
    for
      workflow <- idToWorkflow.checked(workflowPosition.workflowId)
      instr <- workflow.instruction_[A](workflowPosition.position)
    yield
      instr

  /** Returns Left when workflow is missing, returns Gap when position is missing. */
  def instruction(workflowPosition: WorkflowPosition): Checked[Instruction] =
    idToWorkflow.checked(workflowPosition.workflowId).map: workflow =>
      workflow.instruction(workflowPosition.position)

  def instructionIs[I <: Instruction: ClassTag](workflowPosition: WorkflowPosition): Boolean =
    idToWorkflow.get(workflowPosition.workflowId).exists: workflow =>
      implicitClass[I].isAssignableFrom:
        val instr: Instruction = workflow.instruction(workflowPosition.position)
        instr.getClass

  final def isOrderExternalNotVanished(orderId: OrderId): Boolean =
    idToOrder.get(orderId).flatMap(_.externalOrder).exists(o => !o.vanished)
