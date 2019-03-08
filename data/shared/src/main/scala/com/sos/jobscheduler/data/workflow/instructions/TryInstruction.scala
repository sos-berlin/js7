package com.sos.jobscheduler.data.workflow.instructions

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.data.workflow.position.{BranchId, CatchBranchId, TryBranchId, TryCatchBranchId}
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, JsonObject, ObjectEncoder}
import scala.collection.immutable.Seq
import scala.concurrent.duration.{Duration, FiniteDuration}

/**
  * @author Joacim Zschimmer
  */
final case class TryInstruction(
  tryWorkflow: Workflow,
  catchWorkflow: Workflow,
  retryDelays: Seq[FiniteDuration] = Nil)
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

  override def branchWorkflows = (BranchId.Try_ -> tryWorkflow) :: (BranchId.Catch_ -> catchWorkflow) :: Nil

  override def toCatchBranchId(branchId: BranchId) =
    branchId match {
      case TryBranchId(i) => Some(BranchId.catch_(i))
      case _ => super.toCatchBranchId(branchId)
    }

  def retryDelay(index: Int): FiniteDuration =
    if (retryDelays.isEmpty)
      Duration.Zero     // Without retryDelays there is no delay
    else if (index >= retryDelays.length)
      retryDelays.last  // The last given delay is valid for all further iterations
    else
      retryDelays(index - 1)

  override def toString = s"try $tryWorkflow catch $catchWorkflow"
}

object TryInstruction
{
  def toRetryIndex(branchId: BranchId): Checked[Int] =
    branchId match {
      case TryCatchBranchId(i) => Valid(i)
      case _ => Invalid(Problem(s"Invalid BranchId for Try instruction: $branchId"))
    }

  implicit val jsonEncoder: ObjectEncoder[TryInstruction] =
    o => JsonObject(
      "try" -> o.tryWorkflow.asJson,
      "catch" -> o.catchWorkflow.asJson,
      "retryDelays" -> (o.retryDelays.nonEmpty ? o.retryDelays).asJson)

  implicit val jsonDecoder: Decoder[TryInstruction] =
    c => for {
      try_ <- c.get[Workflow]("try")
      catch_ <- c.get[Workflow]("catch")
      delays <- c.get[Option[Seq[FiniteDuration]]]("retryDelays") map (_ getOrElse Nil)
    } yield TryInstruction(try_, catch_, delays)
}
