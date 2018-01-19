package com.sos.jobscheduler.data.workflow

import cats.syntax.either.catsSyntaxEither
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.Position._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class Position(parents: List[Parent], nr: InstructionNr) {

  def /(branchId: BranchId) = new BranchStage(this, branchId)
  def /:(workflowPath: WorkflowPath) = new WorkflowPosition(workflowPath, this)

  def dropChild: Option[Position] =
    for (last ← parents.lastOption) yield
      Position(parents dropRight 1, last.nr)

  def increment: Position = copy(nr = nr + 1)

  override def toString = "#" + (parents map (p ⇒ s"${p.nr.number}/${p.branchId}/") mkString "") + nr.number

  def isNested = parents.nonEmpty

  def depth: Int = parents.size
}

object Position {
  def apply(nr: InstructionNr): Position =
    Position(Nil, nr)

  def apply(nr: Int): Position =
    Position(Nil, nr)

  def apply(parentInstructionNr: Int, branchId: BranchId, nr: Int): Position =
    Position(Parent(parentInstructionNr, branchId) :: Nil, nr)

  final case class Parent(nr: InstructionNr, branchId: BranchId)
  object Parent {
    def apply(nr: InstructionNr, childId: OrderId.ChildId): Parent =
      Parent(nr, BranchId.Named(childId))

    def apply(nr: InstructionNr, childId: String): Parent =
      Parent(nr, BranchId.Named(OrderId.ChildId(childId)))

    def apply(nr: InstructionNr, index: Int): Parent =
      Parent(nr, BranchId.Indexed(index))
  }

  sealed trait BranchId
  object BranchId {
    def apply(childId: OrderId.ChildId): Named = Named(childId)
    implicit def apply(branchId: String): Named = Named(OrderId.ChildId(branchId))
    implicit def apply(index: Int): Indexed = Indexed(index)

    final case class Named(childId: OrderId.ChildId) extends BranchId {
      override def toString = childId.toString
    }
    object Named {
      implicit val jsonEncoder: Encoder[Named] = o ⇒ Json.fromString(o.childId.string)
      implicit val jsonDecoder: Decoder[Named] = _.as[String] map (o ⇒ Named(OrderId.ChildId(o)))
    }

    final case class Indexed(number: Int) extends BranchId {
      override def toString = number.toString
    }
    object Indexed {
      implicit val jsonEncoder: Encoder[Indexed] = o ⇒ Json.fromInt(o.number)
      implicit val jsonDecoder: Decoder[Indexed] = _.as[Int] map Indexed.apply
    }

    implicit val jsonEncoder: Encoder[BranchId] = {
      case o: Named ⇒ o.asJson    // String
      case o: Indexed ⇒ o.asJson  // Number
    }
    implicit val jsonDecoder: Decoder[BranchId] = cursor ⇒
      cursor.as[Named]/*String*/ orElse cursor.as[Indexed]/*Number*/
  }

  final class BranchStage private[Position](position: Position, branchId: BranchId) {
    def /(nr: InstructionNr) = Position(position.parents ::: Parent(position.nr, branchId) :: Nil, nr)
  }

  implicit val jsonEncoder: Encoder[Position] =
    pos ⇒ Json.fromValues(toJsonSeq(pos))

  implicit val jsonDecoder: Decoder[Position] = {
    def decodeParents(pairs: Iterator[Vector[Json]]): Decoder.Result[List[Parent]] = {
      var left: Option[Left[DecodingFailure, Nothing]] = None
      val b = mutable.Buffer[Parent]()
      val parentResults: Iterator[Decoder.Result[Parent]] = pairs map decodeParent
      parentResults foreach {
        case Left(error) ⇒ left = Some(Left(error))
        case Right(parent) ⇒ b += parent
      }
      left getOrElse Right(b.toList)
    }

    def decodeParent(pair: Vector[Json]): Decoder.Result[Parent] =
      for {
        nr ← pair(0).as[InstructionNr]
        branchId ← pair(1).as[BranchId]
      } yield Parent(nr, branchId)

    _.as[Vector[Json]] flatMap (parts ⇒
      if (parts.size % 2 != 1)
        Left(DecodingFailure("Not a valid Position", Nil))
      else
        for {
          parents ← decodeParents(parts dropRight 1 grouped 2)
          nr ← parts.last.as[InstructionNr]
        } yield Position(parents, nr))
  }

  private[workflow] def toJsonSeq(position: Position): Seq[Json] =
    position.parents.toVector.flatMap(p ⇒ Array(p.nr.asJson, p.branchId.asJson)) :+ position.nr.asJson
}
