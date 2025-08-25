package js7.data.workflow.instructions

import io.circe.Codec
import js7.base.circeutils.CirceUtils.deriveRenamingCodec
import js7.base.problem.Checked
import js7.data.agent.AgentPath
import js7.data.source.SourcePos
import js7.data.value.expression.Expression
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Instruction, Workflow}

final case class StickySubagent(
  agentPath: AgentPath,
  subagentBundleIdExpr: Option[Expression],
  subworkflow: Workflow,
  sourcePos: Option[SourcePos])
extends Instruction.WithInstructionBlock:

  def withoutSourcePos: StickySubagent = copy(
    sourcePos = None,
    subworkflow = subworkflow.withoutSourcePos)

  def withPositions(position: Position): StickySubagent =
    copy(
      subworkflow = subworkflow.withPositions(position / BranchId.StickySubagent))

  def adopt(outer: Workflow): StickySubagent = copy(
    subworkflow = subworkflow.copy(outer = Some(outer)))

  def reduceForAgent(agentPath: AgentPath, workflow: Workflow): Instruction =
    copy(
      subworkflow = subworkflow.reduceForAgent(agentPath))

  def withoutBlocks: StickySubagent =
    copy(subworkflow = Workflow.empty)

  def workflow(branchId: BranchId): Checked[Workflow] =
    branchId match
      case BranchId.StickySubagent => Right(subworkflow)
      case _ => unknownBlock(branchId)

  def branchWorkflows: Seq[(BranchId, Workflow)] =
    (BranchId.StickySubagent -> subworkflow) :: Nil

  override def toString = s"stickySubagent $subworkflow$sourcePosToString"


object StickySubagent:

  def apply(
    agentPath: AgentPath,
    subagentBundleIdExpr: Option[Expression] = None)
    (subworkflow: Workflow)
  : StickySubagent =
    new StickySubagent(agentPath, subagentBundleIdExpr, subworkflow, sourcePos = None)

  given Codec.AsObject[StickySubagent] =
    deriveRenamingCodec[StickySubagent](Map(
      "subagentSelectionIdExpr" -> "subagentBundleIdExpr"))
