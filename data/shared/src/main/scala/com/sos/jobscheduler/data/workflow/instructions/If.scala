package com.sos.jobscheduler.data.workflow.instructions

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.workflow.instructions.If._
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
      case Then ⇒ Valid(thenWorkflow)
      case Else ⇒ elseWorkflow toChecked Problem.pure("This If has no 'else' branch")
      case _ ⇒ super.workflow(branchId)
    }

  override def flattenedWorkflows(outer: Position) =
      thenWorkflow.flattenedWorkflowsOf(outer / Then) :::
      elseWorkflow.toList.flatMap(_.flattenedWorkflowsOf(outer / Else))

  override def flattenedInstructions(outer: Position) =
    thenWorkflow.flattenedInstructions(outer / Then) ++
      elseWorkflow.toVector.flatMap(_.flattenedInstructions(outer / Else))

  override def toString = s"if ($predicate) $thenWorkflow" + elseWorkflow.fold("")(w ⇒ s" else $w")
}

object If {
  val Then = BranchId("then")
  val Else = BranchId("else")

  intelliJuseImport(defaultGenericConfiguration)
}
