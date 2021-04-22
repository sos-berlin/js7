package js7.data.workflow.instructions

import io.circe.generic.extras.defaults.defaultGenericConfiguration
import io.circe.generic.extras.{ConfiguredJsonCodec, JsonKey}
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.agent.AgentPath
import js7.data.source.SourcePos
import js7.data.value.expression.Expression
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Instruction, Workflow}

/**
  * @author Joacim Zschimmer
  */
@ConfiguredJsonCodec
final case class If(
  predicate: Expression,
  @JsonKey("then")
  thenWorkflow: Workflow,
  @JsonKey("else")
  elseWorkflow: Option[Workflow] = None,
  sourcePos: Option[SourcePos] = None)
extends Instruction
{
  def withoutSourcePos = copy(
    sourcePos = None,
    thenWorkflow = thenWorkflow.withoutSourcePos,
    elseWorkflow = elseWorkflow.map(_.withoutSourcePos))

  override def withPositions(position: Position): Instruction =
    copy(
      thenWorkflow = thenWorkflow.withPositions(position / BranchId.Then),
      elseWorkflow = elseWorkflow.map(_.withPositions(position / BranchId.Else)))

  override def adopt(outer: Workflow) = copy(
    thenWorkflow = thenWorkflow.copy(outer = Some(outer)),
    elseWorkflow = elseWorkflow.map(_.copy(outer = Some(outer))))

  override def reduceForAgent(agentPath: AgentPath, workflow: Workflow): Instruction =
    copy(
      thenWorkflow = thenWorkflow.reduceForAgent(agentPath),
      elseWorkflow = elseWorkflow.map(_.reduceForAgent(agentPath)))

  override def workflow(branchId: BranchId) =
    branchId match {
      case BranchId.Then => Right(thenWorkflow)
      case BranchId.Else => elseWorkflow toChecked Problem.pure("This If has no 'else' branch")
      case _ => super.workflow(branchId)
    }

  override def branchWorkflows = (BranchId.Then -> thenWorkflow) :: elseWorkflow.map(BranchId.Else -> _).toList

  override def toString = s"if ($predicate) $thenWorkflow" + elseWorkflow.fold("")(w => s" else $w") + sourcePosToString
}

object If {
  intelliJuseImport(defaultGenericConfiguration)
}
