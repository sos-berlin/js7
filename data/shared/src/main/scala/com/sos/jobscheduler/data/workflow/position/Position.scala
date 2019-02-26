package com.sos.jobscheduler.data.workflow.position

import cats.syntax.show.toShow
import com.sos.jobscheduler.data.workflow.WorkflowId
import com.sos.jobscheduler.data.workflow.position.BranchPath.Segment
import io.circe.syntax.EncoderOps
import io.circe.{ArrayEncoder, Decoder, DecodingFailure, Json}
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

  def splitBranchAndNr: Option[(Position, BranchId, InstructionNr)] = {
    for (last <- branchPath.lastOption) yield
      (Position(branchPath.init, last.nr), branchPath.last.branchId, nr)
  }

  def increment: Position =
    copy(nr = nr + 1)

  def asSeq: IndexedSeq[Any] =
    branchPath.toVector.flatMap(p => Array(p.nr.number, p.branchId.toSimpleType)) :+ nr.number

  private[workflow] def asJsonArray: Vector[Json] =
    branchPath.asJsonArray :+ nr.asJson

  override def toString = InstructionNr.Prefix + branchPath.show + nr.number
}

object Position
{
  def apply(nr: InstructionNr): Position =
    Position(Nil, nr)

  def apply(nr: Int): Position =
    Position(Nil, nr)

  def apply(parentInstructionNr: Int, branchId: BranchId, nr: Int): Position =
    Position(Segment(parentInstructionNr, branchId) :: Nil, nr)

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
