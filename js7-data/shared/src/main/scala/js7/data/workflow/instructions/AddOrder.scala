package js7.data.workflow.instructions

import io.circe.Codec
import js7.base.circeutils.CirceUtils.{DecodeWithDefaults, deriveConfiguredCodec}
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.source.SourcePos
import js7.data.value.expression.Expression
import js7.data.workflow.position.{BranchPath, Position, PositionOrLabel}
import js7.data.workflow.{Instruction, WorkflowPath}

final case class AddOrder(
  orderId: Expression,
  workflowPath: WorkflowPath,
  arguments: Map[String, Expression] = Map.empty,
  innerBlock: BranchPath = BranchPath.empty,
  startPosition: Option[Position] = None,
  stopPositions: Set[PositionOrLabel] = Set.empty,
  deleteWhenTerminated: Boolean = false,
  forceJobAdmission: Boolean = false,
  sourcePos: Option[SourcePos] = None)
extends Instruction
{
  def withoutSourcePos = copy(sourcePos = None)
}

object AddOrder
{
  implicit val jsonCodec: Codec.AsObject[AddOrder] = deriveConfiguredCodec
  intelliJuseImport(DecodeWithDefaults)
}
