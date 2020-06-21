package js7.data.workflow.position

import io.circe.{Decoder, DecodingFailure, Json}
import js7.base.utils.ScalaUtils.syntax._
import scala.collection.mutable

/** Denotes globally a branch in a instruction, for example fork or if-then-else, globally unique.
  *
  * @author Joacim Zschimmer
  */
object BranchPath
{
  final case class Segment(nr: InstructionNr, branchId: BranchId)
  {
    def %(position: Position): Position =
      Position(this :: position.branchPath, position.nr)

    def %(nr: InstructionNr): Position =
      Position(this :: Nil, nr)
  }
  object Segment {
    def apply(nr: InstructionNr, branchId: String): Segment =
      Segment(nr, BranchId.Named(branchId))

    def apply(nr: InstructionNr, index: Int): Segment =
      Segment(nr, BranchId.Indexed(index))
  }

  def normalize(branchPath: BranchPath): BranchPath =
    branchPath match {
      case Nil => Nil
      case Segment(nr, branchId) :: tail => nr / branchId.normalized :: normalize(tail)
    }

  private[position] def decodeSegments(pairs: Iterator[List[Json]]): Decoder.Result[BranchPath] = {
    var left: Option[Left[DecodingFailure, Nothing]] = None
    val b = mutable.ListBuffer[Segment]()
    val parentResults: Iterator[Decoder.Result[Segment]] = pairs map decodeSegment
    parentResults foreach {
      case Left(error) => left = Some(Left(error))
      case Right(parent) => b += parent
    }
    left getOrElse Right(b.toList)
  }

  private def decodeSegment(pair: List[Json]): Decoder.Result[Segment] =
    for {
      nr <- pair.head.as[InstructionNr]
      branchId <- pair(1).as[BranchId]
    } yield Segment(nr, branchId)

  object PositionAndBranchId {
    def unapply(branchPath: BranchPath): Option[(Position, BranchId)] =
      branchPath.nonEmpty ? {
        val last = branchPath.last
        (branchPath.dropChild % last.nr, last.branchId)
      }
  }
}
