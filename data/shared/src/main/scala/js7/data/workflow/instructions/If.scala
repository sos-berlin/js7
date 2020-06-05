package js7.data.workflow.instructions

import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.expression.Expression
import js7.data.source.SourcePos
import js7.data.workflow.position.BranchId
import js7.data.workflow.{Instruction, Workflow}
import io.circe.generic.extras.defaults.defaultGenericConfiguration
import io.circe.generic.extras.{ConfiguredJsonCodec, JsonKey}

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

  override def adopt(outer: Workflow) = copy(
    thenWorkflow = thenWorkflow.copy(outer = Some(outer)),
    elseWorkflow = elseWorkflow.map(_.copy(outer = Some(outer))))

  override def workflow(branchId: BranchId) =
    branchId match {
      case BranchId.Then => Right(thenWorkflow)
      case BranchId.Else => elseWorkflow toChecked Problem.pure("This If has no 'else' branch")
      case _ => super.workflow(branchId)
    }

  override def branchWorkflows = (BranchId.Then -> thenWorkflow) :: elseWorkflow.map(BranchId.Else -> _).toList

  override def toString = s"if ($predicate) $thenWorkflow" + elseWorkflow.fold("")(w => s" else $w")
}

object If {
  intelliJuseImport(defaultGenericConfiguration)
}
