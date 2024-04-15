package js7.data.workflow.instructions

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.problem.Checked
import js7.data.agent.AgentPath
import js7.data.source.SourcePos
import js7.data.value.expression.Expression
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Instruction, Workflow}

final case class StickySubagent(
  agentPath: AgentPath,
  subagentSelectionIdExpr: Option[Expression] = None,
  subworkflow: Workflow,
  sourcePos: Option[SourcePos] = None)
extends Instruction:

  def withoutSourcePos: StickySubagent = copy(
    sourcePos = None,
    subworkflow = subworkflow.withoutSourcePos)

  override def withPositions(position: Position): StickySubagent =
    copy(
      subworkflow = subworkflow.withPositions(position / BranchId.StickySubagent))

  override def adopt(outer: Workflow): StickySubagent = copy(
    subworkflow = subworkflow.copy(outer = Some(outer)))

  override def reduceForAgent(agentPath: AgentPath, workflow: Workflow): Instruction =
    copy(
      subworkflow = subworkflow.reduceForAgent(agentPath))

  override def workflow(branchId: BranchId): Checked[Workflow] =
    branchId match
      case BranchId.StickySubagent => Right(subworkflow)
      case _ => super.workflow(branchId)

  override def branchWorkflows: Seq[(BranchId, Workflow)] = (BranchId.StickySubagent -> subworkflow) :: Nil

  override def toString = s"stickySubagent $subworkflow$sourcePosToString"


object StickySubagent:
  implicit val jsonCodec: Codec.AsObject[StickySubagent] =
    deriveCodec[StickySubagent]
