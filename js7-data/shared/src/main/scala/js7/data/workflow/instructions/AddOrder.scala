package js7.data.workflow.instructions

import io.circe.generic.semiauto.deriveCodec
import js7.data.source.SourcePos
import js7.data.value.expression.Expression
import js7.data.value.expression.Expression.ObjectExpression
import js7.data.workflow.{Instruction, WorkflowPath}

final case class AddOrder(
  orderId: Expression,
  workflowPath: WorkflowPath,
  arguments: ObjectExpression = ObjectExpression.empty,
  deleteWhenTerminated: Boolean = false,
  sourcePos: Option[SourcePos] = None)
extends Instruction
{
  def withoutSourcePos = copy(sourcePos = None)
}

object AddOrder
{
  implicit val jsonCodec = deriveCodec[AddOrder]
}
