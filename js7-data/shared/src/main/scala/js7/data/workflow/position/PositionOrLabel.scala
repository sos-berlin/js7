package js7.data.workflow.position

import cats.syntax.show.*
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import js7.base.generic.GenericString
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.reuseIfEqual
import js7.data.workflow.WorkflowId
import js7.data.workflow.position.BranchId.{NoTryBarrierBranchid, nextTryBranchId}
import js7.data.workflow.position.BranchPath.Segment
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait PositionOrLabel


object PositionOrLabel:
  implicit val jsonEncoder: Encoder[PositionOrLabel] =
    case position: Position => position.asJson
    case label: Label => label.asJson

  implicit val jsonDecoder: Decoder[PositionOrLabel] = c =>
    if c.value.isString then
      c.as[Label]
    else
      c.as[Position]

final case class Position(branchPath: BranchPath, nr: InstructionNr)
extends PositionOrLabel:
  def /(branchId: BranchId): BranchPath =
    branchPath ::: Segment(nr, branchId) :: Nil

  def /:(workflowId: WorkflowId) = new WorkflowPosition(workflowId, this)

  def checkedParent: Checked[Position] =
    parent.toRight(Problem.pure(s"Position $toString has no parent position"))

  def parent: Option[Position] =
    for last <- branchPath.lastOption yield
      Position(branchPath.init, last.nr)

  def splitBranchAndNr: Option[(Position, BranchId, InstructionNr)] =
    for last <- branchPath.lastOption yield
      (Position(branchPath.init, last.nr), branchPath.last.branchId, nr)

  def increment: Position =
    copy(nr = nr.increment)

  lazy val normalized: Position =
    reuseIfEqual(this, BranchPath.normalize(branchPath) % nr)

  /** Returns 0 if not in a try/catch-block. */
  def tryCount: Int =
    branchPath.tryCount

  def catchCount: Int =
    branchPath.catchCount

  def nextRetryPosition: Checked[Position] =
    nextRetryBranchPath.map(_ % 0)

  @tailrec
  private[position] def nextRetryBranchPath: Checked[BranchPath] =
    splitBranchAndNr match
      case None => Left(NoTryBlockProblem)
      case Some((parent, branchId @ TryCatchBranchId(_), _)) =>
        nextTryBranchId(branchId) match
          case Right(None) => parent.nextRetryBranchPath
          case Right(Some(tryBranchId)) => Right(parent / tryBranchId)
          case Left(problem) => Left(problem)
      case Some((parent, NoTryBarrierBranchid(()), _)) =>
        parent.nextRetryBranchPath
      case _ =>
        // Everything else is a barrier. For example, Fork.
        // Retry may not be emitted inside a Fork for a Try outside the Fork
        Left(NoTryBlockProblem)

  @tailrec
  def tryPosition: Checked[Position] =
    splitBranchAndNr match
      case None => Left(NoTryBlockProblem)
      case Some((parent, TryCatchBranchId(_), _)) => Right(parent)
      case Some((parent, NoTryBarrierBranchid(()), _)) =>
        parent.tryPosition
      case _ =>
        // Everything else is a barrier. For example, Fork.
        // Retry may not be emitted inside a Fork for a Try outside the Fork
        Left(NoTryBlockProblem)

  def isInFork: Boolean =
    branchPath.exists(_.branchId.isFork)

  def isNestedIn(branchPath: BranchPath): Boolean =
    this.branchPath startsWith branchPath

  /** BranchPath of fork instruction in reverse order. */
  def forkBranchReversed: BranchPath =
    branchPath.view.reverse.dropWhile(o => !o.branchId.isFork).toList

  def toSeq: IndexedSeq[Any] =
    branchPath.view
      .flatMap(p => Seq[Any](p.nr.number, p.branchId.string))
      .toVector :+
      nr.number

  private[workflow] def toJsonSeq: Vector[Json] =
    branchPath.toJsonSeq :+ nr.asJson

  override def toString = branchPath match
    case Nil => nr.number.toString
    case _ => branchPath.show + nr


object Position:
  val First: Position = Position(InstructionNr.First)
  private val NoTryBlockProblem = Problem.pure("Not in a try or catch block")

  def apply(nr: InstructionNr): Position =
    Position(Nil, nr)

  def apply(nr: Int): Position =
    Position(Nil, nr)

  def fromSeq(seq: Seq[Any]): Checked[Position] =
    if seq.isEmpty then
      Left(Problem.pure("Not a valid BranchPath"))
    else
      for
        branchPath <- BranchPath.anySegmentsToCheckedBranchPath(seq dropRight 1 grouped 2)
        nr <- seq.last match
          case i: Int => Right(InstructionNr(i))
          case i: java.lang.Integer => Right(InstructionNr(i))
          case o => Left(Problem(s"Instruction number (integer) expected in Position array instead of: $o"))
      yield Position(branchPath, nr)

  implicit val jsonEncoder: Encoder.AsArray[Position] = _.toJsonSeq

  implicit val jsonDecoder: Decoder[Position] =
    cursor =>
      cursor.value.asArray match
        case None => Left(DecodingFailure("Position must be a JSON array", cursor.history))
        case Some(parts) =>
          if parts.size % 2 == 0 then
            Left(DecodingFailure("Not a valid Position, JSON array size must be 2*n + 1", cursor.history))
          else
            var error: Option[String] = None
            val branchPath = ListBuffer.empty[Segment]
            var lastInstructionNr = -1
            val iterator = parts.iterator
            while error.isEmpty && lastInstructionNr == -1 do
              iterator.next().asNumber.flatMap(_.toInt) match
                case None => error = Some("InstructionNr (a small integer) expected")
                case Some(nr) =>
                  if nr < 0 then
                    error = Some("InstructionNr (a small integer) expected")
                  else if !iterator.hasNext then
                    lastInstructionNr = nr
                  else
                    iterator.next().asString match
                      case None => error = Some("BranchId (a string) expected in position array")
                      case Some(string) =>
                        branchPath += Segment(InstructionNr(nr), BranchId(string))
            error match
              case Some(error) => Left(DecodingFailure(error, cursor.history))
              case None => Right(Position(branchPath.toList, InstructionNr(lastInstructionNr)))


final case class Label private(string: String)
extends PositionOrLabel, GenericString

object Label extends GenericString.Checked_[Label]:
  protected def unchecked(string: String) = new Label(string)

  import scala.language.implicitConversions
  implicit def fromString(label: String): Label = super.apply(label)
