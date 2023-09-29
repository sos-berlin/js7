package js7.data.workflow.instructions

import io.circe.{Codec, Decoder, Encoder}
import io.circe.derivation.{ConfiguredCodec, ConfiguredDecoder, ConfiguredEncoder}
import js7.data.source.SourcePos
import js7.data.value.expression.Expression
import js7.data.workflow.position.{BranchPath, Position, PositionOrLabel}
import js7.data.workflow.{Instruction, WorkflowPath}
import js7.data.workflow.position.BranchPath.syntax.*

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
extends Instruction:
  def withoutSourcePos = copy(sourcePos = None)

object AddOrder:
  import BranchPath.syntax.jsonCodec
  implicit val jsonCodec: Codec.AsObject[AddOrder] = ConfiguredCodec.derive(useDefaults = true)
