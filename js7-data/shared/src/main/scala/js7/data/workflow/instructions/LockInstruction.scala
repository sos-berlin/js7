package js7.data.workflow.instructions

import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Decoder, Encoder}
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.lock.LockPath
import js7.data.order.OrderEvent.LockDemand
import js7.data.source.SourcePos
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Instruction, Workflow}
import org.jetbrains.annotations.TestOnly

final case class LockInstruction private(
  demands: List[LockDemand],
  lockedWorkflow: Workflow,
  sourcePos: Option[SourcePos])
extends Instruction:

  private def checked: Checked[this.type] =
    LockDemand.checked(demands).rightAs(this)

  def withoutSourcePos: LockInstruction = copy(
    sourcePos = None,
    lockedWorkflow = lockedWorkflow.withoutSourcePos)

  override def withPositions(position: Position): LockInstruction =
    copy(
      lockedWorkflow = lockedWorkflow.withPositions(position / BranchId.Lock))

  override def adopt(outer: Workflow): LockInstruction = copy(
    lockedWorkflow = lockedWorkflow.copy(
      outer = Some(outer)))

  override def reduceForAgent(agentPath: AgentPath, workflow: Workflow): Instruction =
    if isVisibleForAgent(agentPath, workflow) then
      copy(
        lockedWorkflow = lockedWorkflow.reduceForAgent(agentPath))
    else
      Gap(sourcePos)  // The agent will never touch this lock or it subworkflow

  def withoutBlocks: LockInstruction =
    copy(lockedWorkflow = Workflow.empty)

  override def workflows: Seq[Workflow] = lockedWorkflow :: Nil

  override def branchWorkflows: Seq[(BranchId, Workflow)] =
    (BranchId.Lock -> lockedWorkflow) :: Nil

  override def workflow(branchId: BranchId): Checked[Workflow] =
    branchId match
      case BranchId.Lock => Right(lockedWorkflow)
      case _ => super.workflow(branchId)

  def lockPaths: List[LockPath] =
    demands.map(_.lockPath)


object LockInstruction:
  @TestOnly
  def apply(
    demands: Seq[LockDemand],
    lockedWorkflow: Workflow,
    sourcePos: Option[SourcePos] = None)
  : LockInstruction =
    new LockInstruction(demands.toList, lockedWorkflow, sourcePos)
      .checked.orThrow

  @TestOnly
  def single(
    lockPath: LockPath,
    count: Option[Int] = None,
    lockedWorkflow: Workflow,
    sourcePos: Option[SourcePos] = None)
  : LockInstruction =
    checked(lockPath, count, lockedWorkflow, sourcePos).orThrow

  def checked(
    lockPath: LockPath,
    count: Option[Int],
    lockedWorkflow: Workflow,
    sourcePos: Option[SourcePos])
  : Checked[LockInstruction] =
    new LockInstruction(LockDemand(lockPath, count) :: Nil, lockedWorkflow, sourcePos)
      .checked

  def checked(locks: Seq[LockDemand], lockedWorkflow: Workflow, sourcePos: Option[SourcePos] = None)
  : Checked[LockInstruction] =
    new LockInstruction(locks.toList, lockedWorkflow, sourcePos)
      .checked

  implicit val jsonEncoder: Encoder.AsObject[LockInstruction] = deriveEncoder

  implicit val jsonDecoder: Decoder[LockInstruction] =
    c => for
      locks <- c.get[List[LockDemand]]("demands")
        /*COMPATIBLE with v2.4*/.orElse(c.value.as[LockDemand].map(_ :: Nil))
      w <- c.get[Workflow]("lockedWorkflow")
      sourcePos <- c.get[Option[SourcePos]]("sourcePos")
      lock <- LockInstruction(locks, w, sourcePos)
        .checked.toDecoderResult(c.history)
    yield lock
