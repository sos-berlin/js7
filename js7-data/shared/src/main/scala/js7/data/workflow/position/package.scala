package js7.data.workflow

import cats.Show
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import js7.base.utils.ScalaUtils.syntax._

/**
  * @author Joacim Zschimmer
  */
package object position
{
  type BranchPath = List[BranchPath.Segment]

  implicit final class RichBranchPath(private val segments: BranchPath) extends AnyVal
  {
    def %(nr: InstructionNr) = Position(segments, nr)

    def %(parent: BranchPath.Segment): BranchPath =
      segments ::: parent :: Nil

    def parent: Option[Position] =
      segments.nonEmpty ? (segments.init % segments.last.nr)

    def dropChild: BranchPath = {
      if (segments.isEmpty) throw new IllegalStateException("dropChild on empty BranchPath ?")
      segments.init
    }

    private[workflow] def toJsonSeq: Vector[Json] =
      segments.view.flatMap(p => Array(p.nr.asJson, p.branchId.asJson)).toVector

    def toSeq: Vector[Any] =
      segments.view.flatMap(p => Array(Int.box(p.nr.number), p.branchId.toSimpleType)).toVector
  }

  implicit val branchPathShow: Show[BranchPath] =
    _.map(p => s"${p.nr.number}/${p.branchId}").mkString(InstructionNr.Prefix)

  implicit val jsonEncoder: Encoder.AsArray[BranchPath] = _.toJsonSeq

  implicit val jsonDecoder: Decoder[BranchPath] =
    cursor => cursor.as[List[Json]].flatMap(parts =>
      if (parts.size % 2 != 0)
        Left(DecodingFailure("Not a valid BranchPath", cursor.history))
      else
        BranchPath.decodeSegments(parts grouped 2))

  implicit final class RichWorkflowId(private val underlying: WorkflowId) extends AnyVal {
    def /(position: Position) = WorkflowPosition(underlying, position)
  }
}
