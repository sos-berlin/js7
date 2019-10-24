package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.instructions.TryInstruction._
import com.sos.jobscheduler.data.workflow.position.{BranchId, CatchBranchId, TryBranchId, TryCatchBranchId}
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import scala.collection.immutable.Seq
import scala.concurrent.duration.{Duration, FiniteDuration}

/**
  * @author Joacim Zschimmer
  */
final case class TryInstruction(
  tryWorkflow: Workflow,
  catchWorkflow: Workflow,
  retryDelays: Option[IndexedSeq[FiniteDuration]] = None,
  maxTries: Option[Int] = None,
  sourcePos: Option[SourcePos] = None)
extends Instruction
{
  def checked: Checked[TryInstruction] =
    if (maxTries exists (_ < 1))
      Left(InvalidMaxTriesProblem)
    else if ((retryDelays.isDefined || maxTries.isDefined) && !containsRetry(catchWorkflow))
      Left(MissingRetryProblem)
    else
      Right(this)

  def withoutSourcePos = copy(
    sourcePos = None,
    tryWorkflow = tryWorkflow.withoutSourcePos,
    catchWorkflow = catchWorkflow.withoutSourcePos)

  override def adopt(outer: Workflow) = copy(
    tryWorkflow = tryWorkflow.copy(outer = Some(outer)),
    catchWorkflow = catchWorkflow.copy(outer = Some(outer)))

  override def workflow(branchId: BranchId) =
    branchId match {
      case TryBranchId(_) => Right(tryWorkflow)
      case CatchBranchId(_) => Right(catchWorkflow)
      case _ => super.workflow(branchId)
    }

  override def branchWorkflows = (BranchId.Try_ -> tryWorkflow) :: (BranchId.Catch_ -> catchWorkflow) :: Nil

  override def toCatchBranchId(branchId: BranchId) =
    branchId match {
      case TryBranchId(i) => Some(BranchId.catch_(i))
      case _ => super.toCatchBranchId(branchId)
    }

  def retryDelay(index: Int): FiniteDuration =
    retryDelays match {
      case None => NoRetryDelay
      case Some(delays) =>
        if (delays.isEmpty)
          NoRetryDelay
        else if (index >= retryDelays.getOrElse(Vector.empty).length)
          delays.last  // The last given delay is valid for all further iterations
        else
          delays(index - 1)
    }

  def isRetry: Boolean =
    catchWorkflow.instructions exists {
      case instr: TryInstruction => containsRetry(instr.tryWorkflow)
      case _: Retry => true
      case instr: If => instr.workflows exists containsRetry
      case _ => false
    }

  override def toString = s"try $tryWorkflow catch $catchWorkflow"
}

object TryInstruction
{
  private val NoRetryDelay = Duration.Zero  // No retryDelays, no delay
  val InvalidMaxTriesProblem = Problem.pure("maxTries argument must be a positive number")
  val MissingRetryProblem = Problem.pure("Missing a retry instruction in the catch block to make sense of retryDelays or maxTries")

  def checked(tryWorkflow: Workflow, catchWorkflow: Workflow, retryDelays: Option[Seq[FiniteDuration]] = None, maxTries: Option[Int] = None,
    sourcePos: Option[SourcePos] = None)
  : Checked[TryInstruction] =
    new TryInstruction(tryWorkflow, catchWorkflow, retryDelays.map(_.toVector), maxTries, sourcePos)
      .checked

  def toRetryIndex(branchId: BranchId): Checked[Int] =
    branchId match {
      case TryCatchBranchId(i) => Right(i)
      case _ => Left(Problem(s"Invalid BranchId for Try instruction: $branchId"))
    }

  private def containsRetry(workflow: Workflow): Boolean =
    workflow.instructions exists {
      case _: Retry => true
      case o: If => o.workflows exists containsRetry
      case o: TryInstruction => containsRetry(o.tryWorkflow)
      case _ => false
    }

  implicit val jsonEncoder: Encoder.AsObject[TryInstruction] =
    o => JsonObject(
      "try" -> o.tryWorkflow.asJson,
      "catch" -> o.catchWorkflow.asJson,
      "retryDelays" -> (o.retryDelays.nonEmpty ? o.retryDelays).asJson,
      "maxTries" -> (o.maxTries.nonEmpty ? o.maxTries).asJson,
      "sourcePos" -> o.sourcePos.asJson)

  implicit val jsonDecoder: Decoder[TryInstruction] =
    c => for {
      try_ <- c.get[Workflow]("try")
      catch_ <- c.get[Workflow]("catch")
      delays <- c.get[Option[Seq[FiniteDuration]]]("retryDelays")
      maxTries <- c.get[Option[Int]]("maxTries")
      sourcePos <- c.get[Option[SourcePos]]("sourcePos")
      tryInstr <- TryInstruction.checked(try_, catch_, delays, maxTries = maxTries, sourcePos).toDecoderResult(c.history)
    } yield tryInstr
}
