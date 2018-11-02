package com.sos.jobscheduler.data.workflow.instructions

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression.BooleanExpression
import com.sos.jobscheduler.data.workflow.{Instruction, Position, Workflow}
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
  def workflow(branchId: Position.BranchId.Indexed): Checked[Workflow] =
    branchId.number match {
      case 0 ⇒ Valid(thenWorkflow)
      case 1 ⇒ elseWorkflow toChecked Problem("This If has no 'else' branch")
      case _ ⇒ Invalid(Problem(s"Invalid index=${branchId.number} for If"))
    }

  override def flattenedInstructions(outer: Position) =
    thenWorkflow.flattenedInstructions(outer / 0) ++
      elseWorkflow.toVector.flatMap(_.flattenedInstructions(outer / 1))

  override def toString = s"If $predicate then $thenWorkflow" +
    elseWorkflow.fold("")(w ⇒ s" else $w")
}

object If {
  intelliJuseImport(defaultGenericConfiguration)
}
