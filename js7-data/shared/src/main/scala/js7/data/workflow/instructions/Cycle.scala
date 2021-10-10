package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.agent.AgentPath
import js7.data.source.SourcePos
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Instruction, Workflow}

final case class Cycle(
  schedule: Schedule,
  cycleWorkflow: Workflow,
  sourcePos: Option[SourcePos] = None)
extends Instruction
{
  def withoutSourcePos = copy(
    sourcePos = None,
    cycleWorkflow = cycleWorkflow.withoutSourcePos)

  override def withPositions(position: Position): Instruction =
    copy(
      cycleWorkflow = cycleWorkflow.withPositions(position / BranchId.Cycle))

  override def adopt(outer: Workflow) = copy(
    cycleWorkflow = cycleWorkflow.copy(
      outer = Some(outer)))

  override def reduceForAgent(agentPath: AgentPath, workflow: Workflow) =
    if (isVisibleForAgent(agentPath, cycleWorkflow))
      copy(
        cycleWorkflow = cycleWorkflow.reduceForAgent(agentPath))
    else
      Gap(sourcePos)  // The agent will never touch this instruction or its subworkflows

  override def workflows =
    cycleWorkflow :: Nil

  override def branchWorkflows =
    (BranchId.Cycle -> cycleWorkflow) :: Nil

  override def workflow(branchId: BranchId) =
    branchId match {
      case BranchId.Cycle => Right(cycleWorkflow)
      case BranchId.Named(string) if string.startsWith(BranchId.CyclePrefix) => Right(cycleWorkflow)
      case _ => super.workflow(branchId)
    }
}

object Cycle
{
  implicit val jsonCodec = deriveCodec[Cycle]

  intelliJuseImport(FiniteDurationJsonDecoder)
}
