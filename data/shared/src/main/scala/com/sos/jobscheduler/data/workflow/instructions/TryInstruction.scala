package com.sos.jobscheduler.data.workflow.instructions

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.base.utils.ScalazStyle._
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
      case Try_ => Valid(tryWorkflow)
      case Catch_ => Valid(catchWorkflow)
      case _ => super.workflow(branchId)
    }

  override def flattenedWorkflows(outer: Position) =
    tryWorkflow.flattenedWorkflowsOf(outer / Try_) ++
    catchWorkflow.flattenedWorkflowsOf(outer / Catch_)

  override def flattenedInstructions(outer: Position) =
    tryWorkflow.flattenedInstructions(outer / Try_) ++
      catchWorkflow.flattenedInstructions(outer / Catch_)

  override def toCatchBranchId(branchId: BranchId) = (branchId == Try_) ? Catch_

  override def toString = s"try $tryWorkflow" + " catch " + catchWorkflow
}

object TryInstruction {
  val Try_ = BranchId("try")
  val Catch_ = BranchId("catch")
  intelliJuseImport(defaultGenericConfiguration)
}
