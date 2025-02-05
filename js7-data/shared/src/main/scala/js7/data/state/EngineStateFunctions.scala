package js7.data.state

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichPartialFunction}
import js7.data.order.{Order, OrderId}
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{Instruction, Workflow, WorkflowId}
import scala.reflect.ClassTag

trait EngineStateFunctions:

  protected def idToWorkflow: PartialFunction[WorkflowId, Workflow]
  protected def idToOrder: PartialFunction[OrderId, Order[Order.State]]

  final def instruction_[A <: Instruction : ClassTag](workflowPosition: WorkflowPosition)
  : Checked[A] =
    idToWorkflow
      .checked(workflowPosition.workflowId)
      .orThrow
      .instruction_[A](workflowPosition.position)

  def instruction(workflowPosition: WorkflowPosition): Instruction =
    idToWorkflow
      .checked(workflowPosition.workflowId)
      .orThrow
      .instruction(workflowPosition.position)

  final def isOrderExternalNotVanished(orderId: OrderId): Boolean =
    idToOrder.get(orderId).flatMap(_.externalOrder).exists(o => !o.vanished)
