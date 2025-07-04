package js7.data.workflow.instructions

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.ZeroDuration
import js7.base.utils.typeclasses.IsEmpty.syntax.*
import js7.data.agent.AgentPath
import js7.data.source.SourcePos
import js7.data.workflow.instructions.TryInstruction.*
import js7.data.workflow.position.*
import js7.data.workflow.{Instruction, Workflow}
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
final case class TryInstruction(
  tryWorkflow: Workflow,
  catchWorkflow: Workflow,
  retryDelays: Option[IndexedSeq[FiniteDuration]] = None,
  maxTries: Option[Int] = None,
  sourcePos: Option[SourcePos] = None)
extends Instruction.WithInstructionBlock:

  override def instructionName = "Try"

  def checked: Checked[TryInstruction] =
    if maxTries exists (_ < 1) then
      Left(InvalidMaxTriesProblem)
    else if (retryDelays.isDefined || maxTries.isDefined) && !containsRetry(catchWorkflow) then
      Left(MissingRetryProblem)
    else
      Right(this)

  def withoutSourcePos: TryInstruction = copy(
    sourcePos = None,
    tryWorkflow = tryWorkflow.withoutSourcePos,
    catchWorkflow = catchWorkflow.withoutSourcePos)

  def withPositions(position: Position): TryInstruction =
    copy(
      tryWorkflow = tryWorkflow.withPositions(position / BranchId.Try_),
      catchWorkflow = catchWorkflow.withPositions(position / BranchId.Catch_))

  def adopt(outer: Workflow): TryInstruction = copy(
    tryWorkflow = tryWorkflow.copy(outer = Some(outer)),
    catchWorkflow = catchWorkflow.copy(outer = Some(outer)))

  def reduceForAgent(agentPath: AgentPath, workflow: Workflow): Instruction =
    copy(
      tryWorkflow = tryWorkflow.reduceForAgent(agentPath),
      catchWorkflow = catchWorkflow.reduceForAgent(agentPath))

  def withoutBlocks: TryInstruction =
    copy(
      tryWorkflow = Workflow.empty,
      catchWorkflow = Workflow.empty)

  def workflow(branchId: BranchId): Checked[Workflow] =
    branchId match
      case TryBranchId(_) => Right(tryWorkflow)
      case CatchBranchId(_) => Right(catchWorkflow)
      case _ => unknownBlock(branchId)

  def branchWorkflows: Seq[(BranchId, Workflow)] =
    (BranchId.Try_ -> tryWorkflow) :: (BranchId.Catch_ -> catchWorkflow) :: Nil

  override def toCatchBranchId(branchId: BranchId): Option[BranchId] =
    branchId match
      case TryBranchId(i) => Some(BranchId.catch_(i))
      case _ => super.toCatchBranchId(branchId)

  def retryDelay(index: Int): FiniteDuration =
    retryDelays match
      case None => NoRetryDelay
      case Some(delays) =>
        if delays.isEmpty then
          NoRetryDelay
        else if index >= retryDelays.getOrElse(Vector.empty).length then
          delays.last  // The last given delay is valid for all further iterations
        else
          delays(index - 1)

  def isRetry: Boolean =
    catchWorkflow.instructions exists:
      case instr: TryInstruction => containsRetry(instr.tryWorkflow)
      case _: Retry => true
      case instr: If => instr.workflows exists containsRetry
      case _ => false

  override def toString = s"try $tryWorkflow catch $catchWorkflow$sourcePosToString"


object TryInstruction:
  private val NoRetryDelay = ZeroDuration  // No retryDelays, no delay
  private val InvalidMaxTriesProblem = Problem.pure("maxTries argument must be a positive number")

  val MissingRetryProblem: Problem =
    Problem.pure("Missing a retry instruction in the catch block to make sense of retryDelays or maxTries")

  def checked(tryWorkflow: Workflow, catchWorkflow: Workflow, retryDelays: Option[Seq[FiniteDuration]] = None, maxTries: Option[Int] = None,
    sourcePos: Option[SourcePos] = None)
  : Checked[TryInstruction] =
    new TryInstruction(tryWorkflow, catchWorkflow, retryDelays.map(_.toVector), maxTries, sourcePos)
      .checked

  def toRetryIndex(branchId: BranchId): Checked[Int] =
    branchId match
      case TryCatchBranchId(i) => Right(i)
      case _ => Left(Problem(s"Invalid BranchId for Try instruction: $branchId"))

  private def containsRetry(workflow: Workflow): Boolean =
    workflow.instructions exists:
      case _: Retry => true
      case o: If => o.workflows exists containsRetry
      case o: TryInstruction => containsRetry(o.tryWorkflow)
      case _ => false

  implicit val jsonEncoder: Encoder.AsObject[TryInstruction] =
    o => JsonObject(
      "try" -> o.tryWorkflow.asJson,
      "catch" -> o.catchWorkflow.asJson,
      "retryDelays" -> o.retryDelays.asJson,
      "maxTries" -> o.maxTries.??.asJson,
      "sourcePos" -> o.sourcePos.asJson)

  implicit val jsonDecoder: Decoder[TryInstruction] =
    c => for
      try_ <- c.get[Workflow]("try")
      catch_ <- c.get[Workflow]("catch")
      delays <- c.get[Option[Seq[FiniteDuration]]]("retryDelays")
      maxTries <- c.get[Option[Int]]("maxTries")
      sourcePos <- c.get[Option[SourcePos]]("sourcePos")
      tryInstr <- TryInstruction.checked(try_, catch_, delays, maxTries = maxTries, sourcePos).toDecoderResult(c.history)
    yield tryInstr
