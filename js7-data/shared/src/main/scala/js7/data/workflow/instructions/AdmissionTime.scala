package js7.data.workflow.instructions

import io.circe.generic.semiauto.deriveCodec
import io.circe.{Codec, Decoder, Encoder}
import js7.base.problem.Checked
import js7.base.time.AdmissionTimeScheme
import js7.data.agent.AgentPath
import js7.data.source.SourcePos
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Instruction, Workflow}

final case class AdmissionTime(
  admissionTimeScheme: AdmissionTimeScheme,
  skipIfNoAdmissionStartForOrderDay: Boolean,
  block: Workflow,
  sourcePos: Option[SourcePos])
extends
  Instruction.WithInstructionBlock:

  def withoutSourcePos: AdmissionTime = copy(
    sourcePos = None,
    block = block.withoutSourcePos)

  def withPositions(position: Position): AdmissionTime =
    copy(
      block = block.withPositions(position / BranchId.AdmissionTime))

  def adopt(outer: Workflow): AdmissionTime = copy(
    block = block.copy(
      outer = Some(outer)))

  def reduceForAgent(agentPath: AgentPath, workflow: Workflow): Instruction =
    if isVisibleForAgent(agentPath, workflow) then
      copy(
        block = block.reduceForAgent(agentPath))
    else
      Gap(sourcePos)  // The agent will never touch this lock or it subworkflow

  def withoutBlocks: AdmissionTime =
    copy(block = Workflow.empty)

  def branchWorkflows: Seq[(BranchId, Workflow)] =
    (BranchId.AdmissionTime -> block) :: Nil

  override def workflows: Seq[Workflow] =
    block :: Nil

  def workflow(branchId: BranchId): Checked[Workflow] =
    branchId match
      case BranchId.AdmissionTime => Right(block)
      case _ => unknownBlock(branchId)


object AdmissionTime:

  def apply(
    admissionTimeScheme: AdmissionTimeScheme,
    skipIfNoAdmissionStartForOrderDay: Boolean = false)
    (block: Workflow)
  : AdmissionTime =
    new AdmissionTime(
      admissionTimeScheme,
      skipIfNoAdmissionStartForOrderDay = skipIfNoAdmissionStartForOrderDay,
      block,
      sourcePos = None)


  given Codec.AsObject[AdmissionTime] = deriveCodec
