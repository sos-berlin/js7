package com.sos.jobscheduler.data.workflow.instructions

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.workflow.instructions.TryInstruction._
import com.sos.jobscheduler.data.workflow.position.{BranchId, Position}
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}
import io.circe.generic.extras.defaults.defaultGenericConfiguration
import io.circe.generic.extras.{ConfiguredJsonCodec, JsonKey}

/**
  * @author Joacim Zschimmer
  */
@ConfiguredJsonCodec
final case class TryInstruction(
  @JsonKey("try")
  tryWorkflow: Workflow,
  @JsonKey("catch")
  catchWorkflow: Workflow)
extends Instruction
{
  override def adopt(outer: Workflow) = copy(
    tryWorkflow = tryWorkflow.copy(outer = Some(outer)),
    catchWorkflow = catchWorkflow.copy(outer = Some(outer)))

  override def workflow(branchId: BranchId) =
    branchId match {
      case BranchId.Indexed(index) ⇒
        index match {
          case 0 ⇒ Valid(tryWorkflow)
          case 1 ⇒ Valid(catchWorkflow)
          case _ ⇒ Invalid(Problem(s"Invalid index=$index for If"))
        }
      case _ ⇒ super.workflow(branchId)
    }

  override def flattenedWorkflows(outer: Position) =
    tryWorkflow.flattenedWorkflowsOf(outer / TryBranchId) ++
    catchWorkflow.flattenedWorkflowsOf(outer / CatchBranchId)

  override def flattenedInstructions(outer: Position) =
    tryWorkflow.flattenedInstructions(outer / TryBranchId) ++
      catchWorkflow.flattenedInstructions(outer / CatchBranchId)

  override def toString = s"try $tryWorkflow" + " catch " + catchWorkflow
}

object TryInstruction {
  val TryBranchId = BranchId.Indexed(0)
  val CatchBranchId = BranchId.Indexed(1)
  intelliJuseImport(defaultGenericConfiguration)
}
