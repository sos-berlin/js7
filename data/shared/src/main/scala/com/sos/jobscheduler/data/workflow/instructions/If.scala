package com.sos.jobscheduler.data.workflow.instructions

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression.BooleanExpression
import com.sos.jobscheduler.data.workflow.position._
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
      case BranchId.Indexed(index) ⇒
        index match {
          case 0 ⇒ Valid(thenWorkflow)
          case 1 ⇒ elseWorkflow toChecked Problem("This If has no 'else' branch")
          case _ ⇒ Invalid(Problem(s"Invalid index=$index for If"))
        }
      case _ ⇒ super.workflow(branchId)
    }

  override def flattenedWorkflows(outer: Position) =
      thenWorkflow.flattenedWorkflowsOf(outer / 0) :::
      elseWorkflow.toList.flatMap(_.flattenedWorkflowsOf(outer / 1))

  override def flattenedInstructions(outer: Position) =
    thenWorkflow.flattenedInstructions(outer / 0) ++
      elseWorkflow.toVector.flatMap(_.flattenedInstructions(outer / 1))

  override def toString = s"if ($predicate) $thenWorkflow" + elseWorkflow.fold("")(w ⇒ s" else $w")
}

object If {
  intelliJuseImport(defaultGenericConfiguration)
}
