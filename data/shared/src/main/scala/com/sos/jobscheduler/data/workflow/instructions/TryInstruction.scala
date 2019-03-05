package com.sos.jobscheduler.data.workflow.instructions

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.workflow.position.{BranchId, CatchBranchId, TryBranchId, TryCatchBranchId}
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
      case TryBranchId(_) => Valid(tryWorkflow)
      case CatchBranchId(_) => Valid(catchWorkflow)
      case _ => super.workflow(branchId)
    }

  override def branchWorkflows = (BranchId.Try_ -> tryWorkflow) :: (BranchId.Catch_ -> catchWorkflow) :: Nil

  override def toCatchBranchId(branchId: BranchId) =
    branchId match {
      case TryBranchId(i) => Some(BranchId.catch_(i))
      case _ => super.toCatchBranchId(branchId)
    }

  override def toString = s"try $tryWorkflow catch $catchWorkflow"
}

object TryInstruction
{
  def toRetryIndex(branchId: BranchId): Checked[Int] =
    branchId match {
      case TryCatchBranchId(i) => Valid(i)
      case _ => Invalid(Problem(s"Invalid BranchId for Try instruction: $branchId"))
    }

  intelliJuseImport(defaultGenericConfiguration)
}
