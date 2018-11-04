package com.sos.jobscheduler.data.workflow.position

import com.sos.jobscheduler.data.workflow.WorkflowId
import com.sos.jobscheduler.data.workflow.position.Position._
import io.circe.syntax.EncoderOps
import io.circe.{ArrayEncoder, Decoder, DecodingFailure, Json}
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class Position(parents: List[Parent], nr: InstructionNr)
{
  def /(branchId: BranchId) = BranchPath.NonEmpty(this, branchId)

  def /:(workflowId: WorkflowId) = new WorkflowPosition(workflowId, this)

  def dropChild: Option[Position] =
    for (last ← parents.lastOption) yield
      Position(parents dropRight 1, last.nr)

  def increment: Position =
    copy(nr = nr + 1)

  def asSeq: IndexedSeq[Any] =
    parents.toVector.flatMap(p ⇒ Array(p.nr.number, p.branchId.toSimpleType)) :+ nr.number

  private[workflow] def asJsonArray: Vector[Json] =
    parents.toVector.flatMap(p ⇒ Array(p.nr.asJson, p.branchId.asJson)) :+ nr.asJson

  override def toString = "#" + (parents map (p ⇒ s"${p.nr.number}/${p.branchId}/") mkString "") + nr.number
}

object Position
{
  def apply(nr: InstructionNr): Position =
    Position(Nil, nr)

  def apply(nr: Int): Position =
    Position(Nil, nr)

  def apply(parentInstructionNr: Int, branchId: BranchId, nr: Int): Position =
    Position(Parent(parentInstructionNr, branchId) :: Nil, nr)

  final case class Parent(nr: InstructionNr, branchId: BranchId)
  object Parent {
    def apply(nr: InstructionNr, branchId: String): Parent =
      Parent(nr, BranchId.Named(branchId))

    def apply(nr: InstructionNr, index: Int): Parent =
      Parent(nr, BranchId.Indexed(index))
  }

  implicit val jsonEncoder: ArrayEncoder[Position] = _.asJsonArray

  implicit val jsonDecoder: Decoder[Position] =
    _.as[List[Json]] flatMap (parts ⇒
      if (parts.size % 2 != 1)
        Left(DecodingFailure("Not a valid Position", Nil))
      else
        for {
          parents ← decodeParents(parts dropRight 1 grouped 2)
          nr ← parts.last.as[InstructionNr]
        } yield Position(parents, nr))

  private[position] def decodeParents(pairs: Iterator[List[Json]]): Decoder.Result[List[Parent]] = {
    var left: Option[Left[DecodingFailure, Nothing]] = None
    val b = mutable.ListBuffer[Parent]()
    val parentResults: Iterator[Decoder.Result[Parent]] = pairs map decodeParent
    parentResults foreach {
      case Left(error) ⇒ left = Some(Left(error))
      case Right(parent) ⇒ b += parent
    }
    left getOrElse Right(b.toList)
  }

  private def decodeParent(pair: List[Json]): Decoder.Result[Parent] =
    for {
      nr ← pair.head.as[InstructionNr]
      branchId ← pair(1).as[BranchId]
    } yield Parent(nr, branchId)
}
