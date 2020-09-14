package js7.data.workflow.position

import io.circe.{Decoder, Json}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import scala.collection.mutable

/** Denotes globally a branch in a instruction, for example fork or if-then-else, globally unique.
  *
  * @author Joacim Zschimmer
  */
object BranchPath
{
  def commonBranchPath(a: BranchPath, b: BranchPath): BranchPath =
    (a, b) match {
      case (aHead :: aTail, bHead :: bTail) if aHead == bHead =>
        aHead :: commonBranchPath(aTail, bTail)
      case _ =>
        Nil
    }

  final case class Segment(nr: InstructionNr, branchId: BranchId)
  {
    def %(position: Position): Position =
      Position(this :: position.branchPath, position.nr)

    def %(nr: InstructionNr): Position =
      Position(this :: Nil, nr)
  }
  object Segment
  {
    def apply(nr: InstructionNr, branchId: String): Segment =
      Segment(nr, BranchId.Named(branchId))
  }

  def normalize(branchPath: BranchPath): BranchPath =
    branchPath match {
      case Nil => Nil
      case Segment(nr, branchId) :: tail => nr / branchId.normalized :: normalize(tail)
    }

  private[position] def decodeSegments(pairs: Iterator[Seq[Json]]): Decoder.Result[BranchPath] =
    genericSeqToCheckedBranchPath(decodeJsonSegment)(pairs)

  /** Converts an sequence of pairs (String, Int) into a Checked[BranchPath]. */
  private[position] def anySegmentsToCheckedBranchPath[L, R](pairs: Iterator[Seq[R]]): Checked[BranchPath] =
    genericSeqToCheckedBranchPath(anySeqToSegment)(pairs)

  private def genericSeqToCheckedBranchPath[L, R](seqToSegment: Seq[R] => Either[L, Segment])(pairs: Iterator[Seq[R]])
  : Either[L, BranchPath] = {
    var left: Option[Left[L, Nothing]] = None
    val buffer = mutable.ListBuffer[Segment]()
    val parentResults: Iterator[Either[L, Segment]] = pairs map seqToSegment
    parentResults foreach {
      case Left(error) => left = Some(Left(error))
      case Right(parent) => buffer += parent
    }
    left getOrElse Right(buffer.toList)
  }

  private def decodeJsonSegment(pair: Seq[Json]): Decoder.Result[Segment] =
    for {
      nr <- pair.head.as[InstructionNr]
      branchId <- pair(1).as[BranchId]
    } yield Segment(nr, branchId)

  private def anySeqToSegment(pair: Seq[Any]): Checked[Segment] =
    for {
      nr <- pair(0) match {
        case i: Int => Right(InstructionNr(i))
        case i: java.lang.Integer => Right(InstructionNr(i))
        case o => Left(Problem(s"Instruction number (integer) expected in Position array instead of: $o"))
      }
      branchId <- pair(1) match {
        case string: String => Right(BranchId(string))
        case o => Left(Problem(s"BranchId (string) expected in Position array instead of: $o"))
      }
    } yield Segment(nr, branchId)

  object PositionAndBranchId {
    def unapply(branchPath: BranchPath): Option[(Position, BranchId)] =
      branchPath.nonEmpty ? {
        val last = branchPath.last
        (branchPath.dropChild % last.nr, last.branchId)
      }
  }
}
