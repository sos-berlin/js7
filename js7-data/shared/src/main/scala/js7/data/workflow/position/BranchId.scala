package js7.data.workflow.position

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import scala.language.implicitConversions

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
}

object BranchId
{
  val Then = BranchId("then")
  val Else = BranchId("else")
  val Try_ = BranchId("try")
  val Catch_ = BranchId("catch")
  val ForkList = BranchId("fork")
  val ForkPrefix = "fork+"
  val Lock = BranchId("lock")

  implicit def apply(branchId: String): Named = Named(branchId)

  def try_(retry: Int): BranchId.Named = {
    require(retry >= 0)
    BranchId(Try_.string + "+" + retry)
  }

  def catch_(retry: Int): BranchId.Named = {
    require(retry >= 0)
    BranchId(Catch_.string + "+" + retry)
  }

  def nextTryBranchId(branchId: BranchId): Checked[Option[BranchId]] =
    branchId match {
      case TryBranchId(_) => Right(None)
      case CatchBranchId(i) => Right(Some(BranchId.try_(i + 1)))
      case _ => Left(Problem(s"Invalid BranchId for nextTryBranchId: $branchId"))
    }

  def fork(branch: String) =
    BranchId(ForkPrefix + branch)

  object IsFailureBoundary
  {
    def unapply(branchId: BranchId): Option[BranchId] =
      branchId.isIsFailureBoundary ? branchId
  }

  final case class Named(string: String) extends BranchId {
    def normalized =
      if (string startsWith "try+") "try"
      else if (string startsWith "catch+") "catch"
      else this

    def isFork = string.startsWith(ForkPrefix) || string == "fork"

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
