package com.sos.jobscheduler.data.workflow.position

import com.sos.jobscheduler.data.workflow.position.Position.{Parent, decodeParents}
import com.sos.jobscheduler.data.workflow.WorkflowId
import io.circe.syntax.EncoderOps
import io.circe.{ArrayEncoder, Decoder, DecodingFailure, Json}

/** Denotes globally a branch in a statement, for example fork or if-then-else, globally unique.
  *
  * @author Joacim Zschimmer
  */
sealed trait BranchPath
{
  def /(nr: InstructionNr): Position

  def /:(workflowId: WorkflowId) = WorkflowBranchPath(workflowId, this)

  final def nonEmpty = !isEmpty

  def isEmpty: Boolean

  protected[workflow] def asJsonArray: Vector[Json]
}

object BranchPath
{
  def fromList(parents: List[Parent]): BranchPath =
    parents match {
      case Nil ⇒ Empty
      case _ ⇒ NonEmpty(Position(parents dropRight 1, parents.last.nr), parents.last.branchId)
    }

  final case class NonEmpty private[position](position: Position, branchId: BranchId) extends BranchPath
  {
    def /(nr: InstructionNr) = Position(asList, nr)

    def isEmpty = false

    def dropLast: BranchPath = position.parents match {
      case Nil ⇒ Empty
      case _ ⇒ NonEmpty(Position(position.parents.dropRight(1), position.nr), position.parents.last.branchId)
    }

    def asList = position.parents ::: Parent(position.nr, branchId) :: Nil

    protected[workflow] def asJsonArray = position.asJsonArray :+ branchId.asJson
  }

  case object Empty extends BranchPath
  {
    def /(nr: InstructionNr) = Position(nr)

    def isEmpty = true

    def asList = Nil

    protected[workflow] def asJsonArray = Vector.empty
  }

  implicit val jsonEncoder: ArrayEncoder[BranchPath] = _.asJsonArray

  implicit val jsonDecoder: Decoder[BranchPath] =
    _.as[List[Json]] flatMap (parts ⇒
      if (parts.size % 2 != 0)
        Left(DecodingFailure("Not a valid BranchPath", Nil))
      else
        decodeParents(parts grouped 2) map fromList)
}
