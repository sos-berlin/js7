package js7.data.workflow.position

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.CycleState
import scala.collection.View
import scala.language.implicitConversions
import scala.util.control.NonFatal

/** Denotes a branch in a instruction, for example fork or if-then-else..
  *
  * @author Joacim Zschimmer
  */
sealed trait BranchId
{
  def string: String

  def normalized: BranchId

  def isFork: Boolean

  final def isIsFailureBoundary = isFork

  /** An order can not be moved through a move boundary. */
  final def isNotMoveBoundary: Boolean =
    BranchId.NoMoveBoundary(this) || BranchId.NoMoveBoundaryPrefixes.exists(string.startsWith)

  def toCycleState: Checked[CycleState]
}

object BranchId
{
  val Then = BranchId("then")
  val Else = BranchId("else")
  val Try_ = BranchId("try")
  val TryPrefix = "try+"
  val Catch_ = BranchId("catch")
  val CatchPrefix = "catch+"
  val ForkList = BranchId("fork")
  val ForkPrefix = "fork+"
  val Lock = BranchId("lock")
  val Cycle = BranchId("cycle")
  val CyclePrefix = "cycle+"
  val ConsumeNotices = BranchId("consumeNotices")
  val StickySubagent = BranchId("stickySubagent")
  val Options = BranchId("options")

  /** Set of BranchIds an Order is movable through. */
  private[BranchId] val NoMoveBoundary = Set[BranchId](Then, Else, Try_, Catch_, Cycle/*???*/, Options)
  private[BranchId] val NoMoveBoundaryPrefixes = Seq(TryPrefix, CatchPrefix, CyclePrefix/*???*/)

  implicit def apply(branchId: String): Named = Named(branchId)

  def try_(retry: Int): BranchId.Named = {
    require(retry >= 0)
    BranchId(TryPrefix + retry)
  }

  def catch_(retry: Int): BranchId.Named = {
    require(retry >= 0)
    BranchId(CatchPrefix + retry)
  }

  def nextTryBranchId(branchId: BranchId): Checked[Option[BranchId]] =
    branchId match {
      case TryBranchId(_) => Right(None)
      case CatchBranchId(i) => Right(Some(BranchId.try_(i + 1)))
      case _ => Left(Problem(s"Invalid BranchId for nextTryBranchId: $branchId"))
    }

  def fork(branch: String) =
    BranchId(ForkPrefix + branch)

  def cycle(cycleState: CycleState): BranchId = {
    val parameters =
      View(
        Some("end="  + cycleState.end.toEpochMilli),
        (cycleState.schemeIndex != 0) ? ("scheme=" + cycleState.schemeIndex),
        (cycleState.periodIndex != 0) ? ("period=" + cycleState.periodIndex),
        (cycleState.index != 0) ? ("i=" + cycleState.index),
        (cycleState.next != Timestamp.Epoch) ? ("next=" + cycleState.next.toEpochMilli)
      ).flatten.mkString(",")

    CyclePrefix + parameters
  }

  object IsFailureBoundary
  {
    def unapply(branchId: BranchId): Option[BranchId] =
      branchId.isIsFailureBoundary ? branchId
  }

  final case class Named(string: String) extends BranchId {
    // TODO Differentiate static and dynamic BranchId (used for static and dynamic call stacks)
    def normalized =
      if (string startsWith TryPrefix) "try"
      else if (string startsWith CatchPrefix) "catch"
      else if (string startsWith CyclePrefix) "cycle"
      else this

    def isFork = string.startsWith(ForkPrefix) || string == "fork"

    def toCycleState: Checked[CycleState] = {
      var CycleState(end, schemeIndex, periodIndex, index, next) = CycleState.empty
      val checked: Checked[Unit] =
        try {
          if (string == "cycle")
            Checked.unit
          else if (!string.startsWith(CyclePrefix))
            Left(Problem.pure(cycleFailed))
          else {
            var checked: Checked[Unit] = Checked.unit
            string.substring(6)
              .split(',')
              .toVector
              .takeWhile(_ => checked.isRight)
              .foreach(part =>
                if (part startsWith "end=")
                  end = Timestamp.ofEpochMilli(part.substring(4).toLong)
                else if (part startsWith "scheme=")
                  schemeIndex = part.substring(7).toInt
                else if (part startsWith "period=")
                  periodIndex = part.substring(7).toInt
                else if (part startsWith "i=")
                  index = part.substring(2).toInt
                else if (part startsWith "next=")
                  next = Timestamp.ofEpochMilli(part.substring(5).toLong)
                else
                  checked = Left(Problem.pure(cycleFailed)))
            checked
          }
        } catch { case NonFatal(t) =>
          Left(Problem.pure(cycleFailed + " - " + t.toStringWithCauses))
        }
      for {
        _ <- checked
        _ <- (end != null) !! Problem("Invalid ")
      } yield CycleState(end, schemeIndex, periodIndex, index, next)
    }

    private def cycleFailed =
      "Expected a Cycle BranchId but got: " + toString

    override def toString = string
  }
  object Named {
    implicit val jsonEncoder: Encoder[Named] = o => Json.fromString(o.string)
    implicit val jsonDecoder: Decoder[Named] = _.as[String] map Named.apply
  }

  implicit val jsonEncoder: Encoder[BranchId] = {
    case o: Named => o.asJson    // String
  }
  implicit val jsonDecoder: Decoder[BranchId] = cursor =>
    cursor.as[Named]
}
