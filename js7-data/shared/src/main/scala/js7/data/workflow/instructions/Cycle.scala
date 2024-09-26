package js7.data.workflow.instructions

import io.circe.Codec
import js7.base.circeutils.CirceUtils.deriveCodecWithDefaults
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.problem.Checked
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.agent.AgentPath
import js7.data.source.SourcePos
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Instruction, Workflow}
import scala.annotation.unused

final case class Cycle private(
  schedule: Schedule,
  cycleWorkflow: Workflow,
  onlyOnePeriod: Boolean = false,
  sourcePos: Option[SourcePos])
extends Instruction:

  def withoutSourcePos: Cycle =
    copy(
      sourcePos = None,
      cycleWorkflow = cycleWorkflow.withoutSourcePos)

  override def withPositions(position: Position): Cycle =
    copy(
      cycleWorkflow = cycleWorkflow.withPositions(position / BranchId.Cycle))

  override def adopt(outer: Workflow): Cycle =
    copy(
      cycleWorkflow = cycleWorkflow.copy(
        outer = Some(outer)))

  override def reduceForAgent(agentPath: AgentPath, workflow: Workflow): Instruction =
    if isVisibleForAgent(agentPath, cycleWorkflow) then
      copy(
        cycleWorkflow = cycleWorkflow.reduceForAgent(agentPath))
    else
      Gap(sourcePos)  // The agent will never touch this instruction or its subworkflows

  override def workflows: Seq[Workflow] =
    cycleWorkflow :: Nil

  def withoutBlocks: Cycle =
    copy(cycleWorkflow = Workflow.empty)

  def branchWorkflows: Seq[(BranchId, Workflow)] =
    (BranchId.Cycle -> cycleWorkflow) :: Nil

  override def workflow(branchId: BranchId): Checked[Workflow] =
    branchId match
      case BranchId.Cycle => Right(cycleWorkflow)
      case BranchId.Named(string) if string.startsWith(BranchId.CyclePrefix) => Right(cycleWorkflow)
      case _ => super.workflow(branchId)


object Cycle:
  given jsonCodec: Codec.AsObject[Cycle] = deriveCodecWithDefaults

  // Dummy, to allow default values in constructor and public apply method.
  @unused
  private def apply(
    schedule: Schedule,
    cycleWorkflow: Workflow,
    onlyOnePeriod: Boolean,
    sourcePos: Option[SourcePos])
  : Cycle = throw NotImplementedError()

  def apply(
    schedule: Schedule,
    onlyOnePeriod: Boolean = false,
    sourcePos: Option[SourcePos] = None)
    (cycleWorkflow: Workflow)
  : Cycle =
    new Cycle(schedule, cycleWorkflow, onlyOnePeriod, sourcePos)

  intelliJuseImport(FiniteDurationJsonDecoder)
