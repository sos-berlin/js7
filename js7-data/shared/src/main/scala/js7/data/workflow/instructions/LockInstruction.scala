package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.problem.Problem
import js7.base.utils.Assertions.assertThat
import js7.data.agent.AgentId
import js7.data.lock.LockId
import js7.data.source.SourcePos
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Instruction, Workflow}

final case class LockInstruction(
  lockId: LockId,
  count: Option[Int],
  lockedWorkflow: Workflow,
  sourcePos: Option[SourcePos] = None)
extends Instruction {

  assertThat(count.forall(_ >= 1))

  def withoutSourcePos = copy(
    sourcePos = None,
    lockedWorkflow = lockedWorkflow.withoutSourcePos)

  override def withPositions(position: Position): Instruction =
    copy(
      lockedWorkflow = lockedWorkflow.withPositions(position / BranchId.Lock))

  override def adopt(outer: Workflow) = copy(
    lockedWorkflow = lockedWorkflow.copy(
      outer = Some(outer)))

  override def reduceForAgent(agentId: AgentId, workflow: Workflow) =
    if (isVisibleForAgent(agentId, workflow))
      copy(
        lockedWorkflow = lockedWorkflow.reduceForAgent(agentId))
    else
      Gap(sourcePos)  // The agent will never touch this lock or it subworkflow

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

  def checked(lockId: LockId, count: Option[Int], lockedWorkflow: Workflow, sourcePos: Option[SourcePos] = None) =
    if (count.exists(_ < 1))
      Left(Problem(s"Invalid count=$count in lock instruction"))
    else
      Right(new LockInstruction(lockId, count, lockedWorkflow, sourcePos))
}
