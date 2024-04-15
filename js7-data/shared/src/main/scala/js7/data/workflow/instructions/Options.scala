package js7.data.workflow.instructions

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.problem.Checked
import js7.data.agent.AgentPath
import js7.data.source.SourcePos
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Instruction, Workflow}

final case class Options(
  stopOnFailure: Option[Boolean] = None,
  block: Workflow,
  sourcePos: Option[SourcePos] = None)
extends Instruction:

  def withoutSourcePos: Options =
    copy(sourcePos = None)

  override def withPositions(position: Position): Options =
    copy(
      block = block.withPositions(position / BranchId.Options))

  override def adopt(outer: Workflow): Options = copy(
    block = block.copy(outer = Some(outer)))

  override def reduceForAgent(agentPath: AgentPath, workflow: Workflow): Instruction =
    copy(
      block = block.reduceForAgent(agentPath))

  override def workflow(branchId: BranchId): Checked[Workflow] =
    branchId match
      case BranchId.Options => Right(block)
      case _ => super.workflow(branchId)

  override def branchWorkflows: Seq[(BranchId, Workflow)] =
    (BranchId.Options -> block) :: Nil


object Options:
  implicit val jsonCodec: Codec.AsObject[Options] = deriveCodec
