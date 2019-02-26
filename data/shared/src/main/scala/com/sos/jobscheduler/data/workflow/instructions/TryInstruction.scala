package com.sos.jobscheduler.data.workflow.instructions

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.workflow.instructions.TryInstruction._
import com.sos.jobscheduler.data.workflow.position.BranchId
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

  override def branchWorkflows = (Try_ -> tryWorkflow) :: (Catch_ -> catchWorkflow) :: Nil

  override def normalizeBranchId(branchId: BranchId) =
    branchId match {
      case TryBranchId(_) => Try_
      case CatchBranchId(_) => Catch_
      case _ => super.normalizeBranchId(branchId)
    }

  override def toCatchBranchId(branchId: BranchId) =
    branchId match {
      case Try_ => Some(Catch_)
      case TryBranchId(i) => Some(BranchId.Indexed(-i))
      case _ => super.toCatchBranchId(branchId)
    }

  override def toString = s"try $tryWorkflow catch $catchWorkflow"
}

object TryInstruction
{
  val Try_ = BranchId("try")
  val Catch_ = BranchId("catch")

  def toRetryIndex(branchId: BranchId): Checked[Int] =
    branchId match {
      case TryBranchId(i) => Valid(i)
      case CatchBranchId(i) => Valid(i)
      case _ => Invalid(Problem(s"Invalid BranchId for Try instruction: $branchId"))
    }

  def nextTryBranchId(branchId: BranchId): Checked[Option[BranchId]] =
    branchId match {
      case TryBranchId(_) => Valid(None)
      case CatchBranchId(i) => Valid(Some(BranchId.Indexed(i + 1)))
      case _ => Invalid(Problem(s"Invalid BranchId for nextTryBranchId: $branchId"))
    }

  private object TryBranchId {
    def unapply(branchId: BranchId): Option[Int] = branchId match {
      case Try_ => Some(0)
      case BranchId.Indexed(i) if i >= 1 => Some(i)
      case _ => None
    }
  }

  private object CatchBranchId {
    def unapply(branchId: BranchId): Option[Int] = branchId match {
      case Catch_ => Some(0)
      case BranchId.Indexed(i) if i <= -1 => Some(-i)
      case _ => None
    }
  }

  intelliJuseImport(defaultGenericConfiguration)
}
