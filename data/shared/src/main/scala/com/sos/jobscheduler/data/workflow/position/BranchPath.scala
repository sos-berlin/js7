package com.sos.jobscheduler.data.workflow.position

import com.sos.jobscheduler.base.utils.ScalazStyle._
import io.circe.{Decoder, DecodingFailure, Json}
import scala.collection.mutable

/** Denotes globally a branch in a statement, for example fork or if-then-else, globally unique.
  *
  * @author Joacim Zschimmer
  */
object BranchPath
{
  final case class Segment(nr: InstructionNr, branchId: BranchId)
  object Segment {
    def apply(nr: InstructionNr, branchId: String): Segment =
      Segment(nr, BranchId.Named(branchId))

    def apply(nr: InstructionNr, index: Int): Segment =
      Segment(nr, BranchId.Indexed(index))
  }

  private[position] def decodeSegments(pairs: Iterator[List[Json]]): Decoder.Result[BranchPath] = {
    var left: Option[Left[DecodingFailure, Nothing]] = None
    val b = mutable.ListBuffer[Segment]()
    val parentResults: Iterator[Decoder.Result[Segment]] = pairs map decodeSegment
    parentResults foreach {
      case Left(error) ⇒ left = Some(Left(error))
      case Right(parent) ⇒ b += parent
    }
    left getOrElse Right(b.toList)
  }

  private def decodeSegment(pair: List[Json]): Decoder.Result[Segment] =
    for {
      nr ← pair.head.as[InstructionNr]
      branchId ← pair(1).as[BranchId]
    } yield Segment(nr, branchId)
}
