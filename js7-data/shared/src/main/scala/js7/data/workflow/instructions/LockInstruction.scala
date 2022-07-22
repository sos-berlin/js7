package js7.data.workflow.instructions

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.problem.Problem
import js7.base.utils.Assertions.assertThat
import js7.data.agent.AgentPath
import js7.data.lock.LockPath
import js7.data.source.SourcePos
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Instruction, Workflow}

final case class LockInstruction(
  lockPath: LockPath,
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

  override def reduceForAgent(agentPath: AgentPath, workflow: Workflow) =
    if (isVisibleForAgent(agentPath, workflow))
      copy(
        lockedWorkflow = lockedWorkflow.reduceForAgent(agentPath))
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
  implicit val jsonCodec: Codec.AsObject[LockInstruction] = deriveCodec[LockInstruction]

  def checked(lockPath: LockPath, count: Option[Int], lockedWorkflow: Workflow, sourcePos: Option[SourcePos] = None) =
    if (count.exists(_ < 1))
      Left(Problem(s"Invalid count=$count in lock instruction"))
    else
      Right(new LockInstruction(lockPath, count, lockedWorkflow, sourcePos))
}
