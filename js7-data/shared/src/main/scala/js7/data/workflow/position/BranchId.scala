package js7.data.workflow.position

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.Nulls.nonNull
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.CycleState
import scala.collection.View
import scala.language.implicitConversions
import scala.util.control.NonFatal

/** Denotes a branch in a instruction, for example fork or if-then-else..
  *
  * @author Joacim Zschimmer
  */
sealed trait BranchId:
  def string: String

  def normalized: BranchId

  def isFork: Boolean

  def isCycle: Boolean

  final def isIsFailureBoundary: Boolean = isFork

  /** An order can not be moved through a move boundary. */
  final def isNotMoveBoundary: Boolean =
    BranchId.NoMoveBoundary(this) || BranchId.NoMoveBoundaryPrefixes.exists(string.startsWith)

  def toCycleState: Checked[CycleState]


object BranchId:
  val Then: Named = BranchId("then")
  val ThenPrefix = "then+"
  val Else: Named = BranchId("else")
  val Try_ : Named = BranchId("try")
  val TryPrefix = "try+"
  val Catch_ : Named = BranchId("catch")
  val CatchPrefix = "catch+"
  val ForkList: Named = BranchId("fork")
  val ForkPrefix = "fork+"
  val Lock: Named = BranchId("lock")
  val Cycle: Named = BranchId("cycle")
  val CyclePrefix = "cycle+"
  val ConsumeNotices: Named = BranchId("consumeNotices")
  val StickySubagent: Named = BranchId("stickySubagent")
  val Options: Named = BranchId("options")

  /** Set of BranchIds an Order is movable through. */
  private[BranchId] val NoMoveBoundary = Set[BranchId](Then, Else, Try_, Catch_, Cycle/*???*/, Options)
  private[BranchId] val NoMoveBoundaryPrefixes = Seq(TryPrefix, CatchPrefix, CyclePrefix/*???*/)

  implicit def apply(branchId: String): Named = Named(branchId)

  def then_(index: Int): BranchId.Named =
    if index == 1 then
      Then
    else
      BranchId(s"then+$index")

  def try_(retry: Int): BranchId.Named =
    require(retry >= 0)
    BranchId(TryPrefix + retry)

  def catch_(retry: Int): BranchId.Named =
    require(retry >= 0)
    BranchId(CatchPrefix + retry)

  def nextTryBranchId(branchId: BranchId): Checked[Option[BranchId]] =
    branchId match
      case TryBranchId(_) => Right(None)
      case CatchBranchId(i) => Right(Some(BranchId.try_(i + 1)))
      case _ => Left(Problem(s"Invalid BranchId for nextTryBranchId: $branchId"))

  def fork(branch: String): Named =
    BranchId(ForkPrefix + branch)

  def cycle(cycleState: CycleState): BranchId =
    val parameters =
      View(
        Some("end="  + cycleState.end.toEpochMilli),
        (cycleState.schemeIndex != 0) ? ("scheme=" + cycleState.schemeIndex),
        (cycleState.periodIndex != 0) ? ("period=" + cycleState.periodIndex),
        (cycleState.index != 0) ? ("i=" + cycleState.index),
        (cycleState.next != Timestamp.Epoch) ? ("next=" + cycleState.next.toEpochMilli)
      ).flatten.mkString(",")

    CyclePrefix + parameters

  object IsFailureBoundary:
    def unapply(branchId: BranchId): Option[BranchId] =
      branchId.isIsFailureBoundary ? branchId

  final case class Named(string: String) extends BranchId:
    // TODO Differentiate static and dynamic BranchId (used for static and dynamic call stacks)
    def normalized: BranchId =
      if string.startsWith(TryPrefix) then "try"
      else if string.startsWith(CatchPrefix) then "catch"
      else if string.startsWith(CyclePrefix) then "cycle"
      else this

    def isFork: Boolean =
      string.startsWith(ForkPrefix) || string == "fork"

    def isCycle: Boolean =
      string.startsWith(BranchId.CyclePrefix) || this == Cycle

    def toCycleState: Checked[CycleState] =
      var CycleState(end, schemeIndex, periodIndex, index, next) = CycleState.empty
      for
        _ <-
          try
            if string == "cycle" then
              Checked.unit
            else if !string.startsWith(CyclePrefix) then
              Left(Problem.pure(cycleFailed))
            else
              var checked: Checked[Unit] = Checked.unit
              string.substring(6)
                .split(',')
                .toVector
                .takeWhile(_ => checked.isRight)
                .foreach(part =>
                  if part.startsWith("end=") then
                    end = Timestamp.ofEpochMilli(part.substring(4).toLong)
                  else if part.startsWith("scheme=") then
                    schemeIndex = part.substring(7).toInt
                  else if part.startsWith("period=") then
                    periodIndex = part.substring(7).toInt
                  else if part.startsWith("i=") then
                    index = part.substring(2).toInt
                  else if part.startsWith("next=") then
                    next = Timestamp.ofEpochMilli(part.substring(5).toLong)
                  else
                    checked = Left(Problem.pure(cycleFailed)))
              checked
          catch case NonFatal(t) =>
            Left(Problem.pure(cycleFailed + " - " + t.toStringWithCauses))
        _ <- nonNull(end) !! Problem.pure(cycleFailed) // null ???
      yield
        CycleState(end, schemeIndex, periodIndex, index, next)

    private def cycleFailed =
      "Expected a Cycle BranchId but got: " + toString

    override def toString = string

  object Named:
    implicit val jsonEncoder: Encoder[Named] = o => Json.fromString(o.string)
    implicit val jsonDecoder: Decoder[Named] = _.as[String] map Named.apply


  object NoTryBarrierBranchid:
    def unapply(branchId: BranchId): Option[Unit] =
      branchId match
        case BranchId.Then | BranchId.Else | BranchId.Options => Some(())
        case _ => None


  implicit val jsonEncoder: Encoder[BranchId] =
    case o: Named => o.asJson    // String
  implicit val jsonDecoder: Decoder[BranchId] = cursor =>
    cursor.as[Named]
