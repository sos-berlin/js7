package com.sos.jobscheduler.data.workflow

import cats.Show
import io.circe.syntax.EncoderOps
import io.circe.{ArrayEncoder, Decoder, DecodingFailure, Json}

/**
  * @author Joacim Zschimmer
  */
package object position
{
  type BranchPath = List[BranchPath.Segment]

  implicit final class RichBranchPath(private val segments: BranchPath) extends AnyVal
  {
    def /(nr: InstructionNr) = Position(segments, nr)

    def /(parent: BranchPath.Segment): BranchPath =
      segments ::: parent :: Nil

    def dropChild: BranchPath = {
      if (segments.isEmpty) throw new IllegalStateException("dropChild on empty BranchPath ?")
      segments dropRight 1
    }

    private[workflow] def asJsonArray: Vector[Json] =
      segments.toVector.flatMap(p ⇒ Array(p.nr.asJson, p.branchId.asJson))
  }

  implicit val branchPathShow: Show[BranchPath] =
    segments ⇒ segments map (p ⇒ s"${p.nr.number}/${p.branchId}/") mkString ""

  implicit val jsonEncoder: ArrayEncoder[BranchPath] = _.asJsonArray

  implicit val jsonDecoder: Decoder[BranchPath] =
    _.as[List[Json]] flatMap (parts ⇒
      if (parts.size % 2 != 0)
        Left(DecodingFailure("Not a valid BranchPath", Nil))
      else
        BranchPath.decodeSegments(parts grouped 2))
}
