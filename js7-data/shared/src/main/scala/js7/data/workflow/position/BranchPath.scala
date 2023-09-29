package js7.data.workflow.position

import cats.Show
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, HCursor, Json}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import scala.annotation.tailrec
import scala.collection.{View, mutable}

type BranchPath = List[BranchPath.Segment]

/** Denotes globally a branch in a instruction, for example fork or if-then-else, globally unique.
  *
  * @author Joacim Zschimmer
  */
object BranchPath
{
  val empty: BranchPath = Nil

  def fromSeq(seq: Seq[Any]): Checked[BranchPath] =
    BranchPath.anySegmentsToCheckedBranchPath(seq grouped 2)

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

  private[position] def decodeSegments(pairs: Iterator[Seq[Json]], cursor: HCursor): Decoder.Result[BranchPath] =
    genericSeqToBranchPath(decodeJsonSegment(_, cursor))(pairs)

  /** Converts an sequence of pairs (String, Int) into a Checked[BranchPath]. */
  private[position] def anySegmentsToCheckedBranchPath[L, R](pairs: Iterator[Seq[R]]): Checked[BranchPath] =
    genericSeqToBranchPath(anySeqToSegment)(pairs)

  private def genericSeqToBranchPath[L, R](seqToSegment: Seq[R] => Either[L, Segment])(pairs: Iterator[Seq[R]])
  : Either[L, BranchPath] = {
    var left: Option[Left[L, Nothing]] = None
    val buffer = mutable.ListBuffer.empty[Segment]
    val parentResults: Iterator[Either[L, Segment]] = pairs map seqToSegment
    parentResults foreach {
      case Left(error) => left = Some(Left(error))
      case Right(parent) => buffer += parent
    }
    left getOrElse Right(buffer.toList)
  }

  private def decodeJsonSegment(pair: Seq[Json], cursor: HCursor): Decoder.Result[Segment] =
    if (pair.sizeIs != 2)
      Left(DecodingFailure("Not a valid BranchPath", cursor.history))
    else
      for {
        nr <- pair.head.as[InstructionNr]
        branchId <- pair(1).as[BranchId]
      } yield Segment(nr, branchId)

  private def anySeqToSegment(pair: Seq[Any]): Checked[Segment] =
    if (pair.sizeIs != 2)
      Left(Problem.pure("Not a valid BranchPath"))
    else
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
    import BranchPath.syntax.*
    def unapply(branchPath: BranchPath): Option[(Position, BranchId)] =
      branchPath.nonEmpty ? {
        val last = branchPath.last
        (branchPath.dropChild % last.nr, last.branchId)
      }
  }

  object syntax {
    extension (segments: BranchPath) {
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
        segments.view.flatMap(p => View(p.nr.asJson, p.branchId.asJson)).toVector

      def toFlatSeq: Vector[Any] =
        segments.view.flatMap(p => View(Int.box(p.nr.number), p.branchId.string)).toVector

      def dropLastBranchId: Position =
        segments.init % segments.last.nr

      /** Returns 0 if not in a try/catch-block. */
      def tryCount: Int =
        calculateTryCount(segments.reverse)

      /** Returns 0 if not in a try/catch-block. */
      def catchCount: Int =
        calculateCatchCount(segments.reverse)
    }

    implicit val branchPathShow: Show[BranchPath] =
      _.map(p => s"${p.nr.number}/${p.branchId}").mkString(InstructionNr.Prefix)

    implicit val jsonCodec: Codec[BranchPath] = {
      val jsonEncoder: Encoder.AsArray[BranchPath] = _.toJsonSeq

      val jsonDecoder: Decoder[BranchPath] =
        cursor => cursor.as[List[Json]].flatMap(parts =>
          BranchPath.decodeSegments(parts grouped 2, cursor))

      Codec.from(jsonDecoder, jsonEncoder)
    }

    @tailrec
    private def calculateTryCount(reverseBranchPath: List[Segment]): Int =
      reverseBranchPath match {
        case Segment(_, TryCatchBranchId(retry)) :: _ => retry + 1
        case Segment(_, BranchId.Then | BranchId.Else) :: prefix => calculateTryCount(prefix)
        case _ => 0  // Not in a try/catch
      }

    @tailrec
    private def calculateCatchCount(reverseBranchPath: List[Segment]): Int =
      reverseBranchPath match {
        case Segment(_, TryBranchId(retry)) :: _ => retry
        case Segment(_, CatchBranchId(retry)) :: _ =>  retry + 1
        case Segment(_, BranchId.Then | BranchId.Else) :: prefix => calculateCatchCount(prefix)
        case _ => 0  // Not in a try/catch
      }
  }
}
