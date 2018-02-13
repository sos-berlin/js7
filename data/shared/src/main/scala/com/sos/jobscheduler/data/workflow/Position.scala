package com.sos.jobscheduler.data.workflow

import cats.syntax.either.catsSyntaxEither
import com.sos.jobscheduler.data.workflow.Position._
import io.circe.syntax.EncoderOps
import io.circe.{ArrayEncoder, Decoder, DecodingFailure, Encoder, Json}
import scala.collection.mutable
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class Position(parents: List[Parent], nr: InstructionNr) {

  def /(branchId: BranchId) = Parents.NonEmpty(this, branchId)
  def /:(workflowPath: WorkflowPath) = new WorkflowPosition(workflowPath, this)

  def dropChild: Option[Position] =
    for (last ← parents.lastOption) yield
      Position(parents dropRight 1, last.nr)

  def increment: Position = copy(nr = nr + 1)

  override def toString = "#" + (parents map (p ⇒ s"${p.nr.number}/${p.branchId}/") mkString "") + nr.number
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
    def apply(nr: InstructionNr, branchId: String): Parent =
      Parent(nr, BranchId.Named(branchId))

    def apply(nr: InstructionNr, index: Int): Parent =
      Parent(nr, BranchId.Indexed(index))
  }

  sealed trait BranchId
  object BranchId {
    implicit def apply(branchId: String): Named = Named(branchId)
    implicit def apply(index: Int): Indexed = Indexed(index)

    final case class Named(string: String) extends BranchId {
      override def toString = string
    }
    object Named {
      implicit val jsonEncoder: Encoder[Named] = o ⇒ Json.fromString(o.string)
      implicit val jsonDecoder: Decoder[Named] = _.as[String] map Named.apply
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

  sealed trait Parents {
    def /(nr: InstructionNr): Position
  }
  object Parents {
    final case class NonEmpty private[Position](position: Position, branchId: BranchId) extends Parents {
      def /(nr: InstructionNr) = Position(position.parents ::: Parent(position.nr, branchId) :: Nil, nr)
    }
    case object Empty extends Parents {
      def /(nr: InstructionNr) = Position(nr)
    }
  }

  implicit val jsonEncoder: ArrayEncoder[Position] =
    position ⇒ position.parents.toVector.flatMap(p ⇒ Array(p.nr.asJson, p.branchId.asJson)) :+ position.nr.asJson

  implicit val jsonDecoder: Decoder[Position] =
    _.as[List[Json]] flatMap (parts ⇒
      if (parts.size % 2 != 1)
        Left(DecodingFailure("Not a valid Position", Nil))
      else
        for {
          parents ← decodeParents(parts dropRight 1 grouped 2)
          nr ← parts.last.as[InstructionNr]
        } yield Position(parents, nr))

  private def decodeParents(pairs: Iterator[List[Json]]): Decoder.Result[List[Parent]] = {
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
