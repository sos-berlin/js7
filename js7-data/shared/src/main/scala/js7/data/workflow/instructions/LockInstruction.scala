package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.data.lock.LockId
import js7.data.source.SourcePos
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Instruction, Workflow}

final case class LockInstruction(
  lockId: LockId,
  exclusive: Boolean,
  lockedWorkflow: Workflow,
  sourcePos: Option[SourcePos] = None)
extends Instruction {

  def withoutSourcePos = copy(
    sourcePos = None,
    lockedWorkflow = lockedWorkflow.withoutSourcePos)

  override def withPositions(position: Position): Instruction =
    copy(
      lockedWorkflow = lockedWorkflow.withPositions(position / BranchId.Lock))

  override def workflows = lockedWorkflow :: Nil

  override def branchWorkflows =
    (BranchId.Lock -> lockedWorkflow) :: Nil

  override def workflow(branchId: BranchId) =
    branchId match {
      case BranchId.Lock => Right(lockedWorkflow)
      case _ => super.workflow(branchId)
    }
}

object LockInstruction {
  implicit val jsonCodec = deriveCodec[LockInstruction]
}
