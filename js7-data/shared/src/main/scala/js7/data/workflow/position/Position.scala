package js7.data.workflow.position

import cats.syntax.show.toShow
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.reuseIfEqual
import js7.data.workflow.WorkflowId
import js7.data.workflow.position.BranchId.nextTryBranchId
import js7.data.workflow.position.BranchPath.Segment
import js7.data.workflow.position.Position._
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
final case class Position(branchPath: BranchPath, nr: InstructionNr)
{
  def /(branchId: BranchId): BranchPath =
    branchPath ::: Segment(nr, branchId) :: Nil

  def /:(workflowId: WorkflowId) = new WorkflowPosition(workflowId, this)

  def dropChild: Option[Position] =
    splitBranchAndNr.map(_._1)

  def splitBranchAndNr: Option[(Position, BranchId, InstructionNr)] =
    for (last <- branchPath.lastOption) yield
      (Position(branchPath.init, last.nr), branchPath.last.branchId, nr)

  def increment: Position =
    copy(nr = nr + 1)

  lazy val normalized: Position =
    reuseIfEqual(this, BranchPath.normalize(branchPath) % nr)

  /** Returns 0 if not in a try/catch-block. */
  lazy val tryCount: Int = calculateTryCount

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

  @tailrec
  def nextRetryBranchPath: Checked[BranchPath] =
    splitBranchAndNr match {
      case None => Left(NoTryBlockProblem)
      case Some((parent, branchId @ TryCatchBranchId(_), _)) =>
        nextTryBranchId(branchId) match {
          case Right(None) => parent.nextRetryBranchPath
          case Right(Some(tryBranchId)) => Right(parent / tryBranchId)
          case Left(l) => Left(l)
        }
      case Some((parent, BranchId.Then | BranchId.Else, _)) =>
        parent.nextRetryBranchPath
      case _ => Left(NoTryBlockProblem) // For example, Fork is a barrier. Retry may not be emitted inside a Fork for a Try outside the Fork
    }

  def isInFork = branchPath.exists(_.branchId.isFork)

  /** BranchPath of fork instruction in reverse order. */
  def forkBranchReversed: BranchPath =
    branchPath.reverse.dropWhile(o => !o.branchId.isFork)

  def toSeq: IndexedSeq[Any] =
    branchPath.view.flatMap(p => Seq(p.nr.number, p.branchId.toSimpleType)).toVector :+ nr.number

  private[workflow] def toJsonSeq: Vector[Json] =
    branchPath.toJsonSeq :+ nr.asJson

  override def toString = branchPath match {
    case Nil => nr.number.toString
    case _ => branchPath.show + nr
  }
}

object Position
{
  val First = Position(InstructionNr.First)
  private val NoTryBlockProblem = Problem.pure("Retry, but not in a catch-block")

  def apply(nr: InstructionNr): Position =
    Position(Nil, nr)

  def apply(nr: Int): Position =
    Position(Nil, nr)

  def fromSeq(seq: Seq[Any]): Checked[Position] =
    if (seq.size % 2 != 1)
      Left(Problem.pure("Position sequence muss be of uneven length"))
    else
      for {
        branchPath <- BranchPath.anySegmentsToCheckedBranchPath(seq dropRight 1 grouped 2)
        nr <- seq.last match {
          case i: Int => Right(InstructionNr(i))
          case i: java.lang.Integer => Right(InstructionNr(i))
          case o => Left(Problem(s"Instruction number (integer) expected in Position array instead of: $o"))
        }
      } yield Position(branchPath, nr)

  implicit val jsonEncoder: Encoder.AsArray[Position] = _.toJsonSeq

  implicit val jsonDecoder: Decoder[Position] =
    cursor => cursor.as[List[Json]].flatMap(parts =>
      if (parts.size % 2 != 1)
        Left(DecodingFailure("Not a valid Position", cursor.history))
      else
        for {
          branchPath <- BranchPath.decodeSegments(parts dropRight 1 grouped 2)
          nr <- parts.last.as[InstructionNr]
        } yield Position(branchPath, nr))
}
