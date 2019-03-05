package com.sos.jobscheduler.data.workflow.position

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.show.toShow
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalaUtils.reuseIfEqual
import com.sos.jobscheduler.data.workflow.WorkflowId
import com.sos.jobscheduler.data.workflow.position.BranchId.nextTryBranchId
import com.sos.jobscheduler.data.workflow.position.BranchPath.Segment
import com.sos.jobscheduler.data.workflow.position.Position._
import io.circe.syntax.EncoderOps
import io.circe.{ArrayEncoder, Decoder, DecodingFailure, Json}
import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq

/**
  * @author Joacim Zschimmer
  */
final case class Position(branchPath: BranchPath, nr: InstructionNr)
{
  def /(branchId: BranchId): BranchPath =
    branchPath ::: Segment(nr, branchId) :: Nil

  def /:(workflowId: WorkflowId) = new WorkflowPosition(workflowId, this)

  def dropChild: Option[Position] =
    splitBranchAndNr map (_._1)

  def splitBranchAndNr: Option[(Position, BranchId, InstructionNr)] =
    for (last <- branchPath.lastOption) yield
      (Position(branchPath.init, last.nr), branchPath.last.branchId, nr)

  def increment: Position =
    copy(nr = nr + 1)

  lazy val normalized: Position =
    reuseIfEqual(this, BranchPath.normalize(branchPath) % nr)

  // not used
  /** Returns 0 if not in a try/catch-block. */
  private[position] lazy val tryCount: Int = calculateTryCount

  @tailrec
  private def calculateTryCount: Int =
    splitBranchAndNr match {
      case Some((_, TryCatchBranchId(retry), _)) => retry + 1
      case Some((parentPos, BranchId.Then | BranchId.Else, _)) => parentPos.calculateTryCount
      case _ => 0  // Not in a try/catch
    }

  /** Returns 0 if not in a try/catch-block. */
  lazy val catchCount: Int = calculateCatchCount

  @tailrec
  private def calculateCatchCount: Int =
    splitBranchAndNr match {
      case Some((_, TryBranchId(retry), _)) => retry
      case Some((_, CatchBranchId(retry), _)) => retry + 1
      case Some((parentPos, BranchId.Then | BranchId.Else, _)) => parentPos.calculateCatchCount
      case _ => 0  // Not in a try/catch
    }

  def nextRetryPosition: Checked[Position] =
    nextRetryBranchPath.map(_ % 0)

  @tailrec
  private def nextRetryBranchPath: Checked[BranchPath] =
    splitBranchAndNr match {
      case None => Invalid(NoTryBlockProblem)
      case Some((parent, branchId @ TryCatchBranchId(_), _)) =>
        nextTryBranchId(branchId) match {
          case Valid(None) => parent.nextRetryBranchPath
          case Valid(Some(tryBranchId)) => Valid(parent / tryBranchId)
          case o @ Invalid(_) => o
        }
      case Some((parent, BranchId.Then | BranchId.Else, _)) =>
        parent.nextRetryBranchPath
      case _ => Invalid(NoTryBlockProblem) // For example, Fork is a barrier. Retry may not be issued inside a Fork for a Try outside the Fork
    }

  def asSeq: IndexedSeq[Any] =
    branchPath.toVector.flatMap(p => Array(p.nr.number, p.branchId.toSimpleType)) :+ nr.number

  private[workflow] def asJsonArray: Vector[Json] =
    branchPath.asJsonArray :+ nr.asJson

  override def toString = branchPath match {
    case Nil => nr.number.toString
    case _ => branchPath.show + nr
  }
}

object Position
{
  private val NoTryBlockProblem = Problem.pure("Not in a catch-block")

  def apply(nr: InstructionNr): Position =
    Position(Nil, nr)

  def apply(nr: Int): Position =
    Position(Nil, nr)

  implicit val jsonEncoder: ArrayEncoder[Position] = _.asJsonArray

  implicit val jsonDecoder: Decoder[Position] =
    _.as[List[Json]] flatMap (parts =>
      if (parts.size % 2 != 1)
        Left(DecodingFailure("Not a valid Position", Nil))
      else
        for {
          branchPath <- BranchPath.decodeSegments(parts dropRight 1 grouped 2)
          nr <- parts.last.as[InstructionNr]
        } yield Position(branchPath, nr))
}
