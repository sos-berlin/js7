package com.sos.jobscheduler.data.workflow.instructions

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression.BooleanExpression
import com.sos.jobscheduler.data.workflow.position.BranchId
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}
import io.circe.generic.extras.defaults.defaultGenericConfiguration
import io.circe.generic.extras.{ConfiguredJsonCodec, JsonKey}

/**
  * @author Joacim Zschimmer
  */
@ConfiguredJsonCodec
final case class If(
  predicate: BooleanExpression,
  @JsonKey("then")
  thenWorkflow: Workflow,
  @JsonKey("else")
  elseWorkflow: Option[Workflow] = None)
extends Instruction
{
  override def adopt(outer: Workflow) = copy(
    thenWorkflow = thenWorkflow.copy(outer = Some(outer)),
    elseWorkflow = elseWorkflow.map(_.copy(outer = Some(outer))))

  override def workflow(branchId: BranchId) =
    branchId match {
      case BranchId.Then => Valid(thenWorkflow)
      case BranchId.Else => elseWorkflow toChecked Problem.pure("This If has no 'else' branch")
      case _ => super.workflow(branchId)
    }

  override def branchWorkflows = (BranchId.Then -> thenWorkflow) :: elseWorkflow.map(BranchId.Else -> _).toList

  override def toString = s"if ($predicate) $thenWorkflow" + elseWorkflow.fold("")(w => s" else $w")
}

object If {
  intelliJuseImport(defaultGenericConfiguration)
}
