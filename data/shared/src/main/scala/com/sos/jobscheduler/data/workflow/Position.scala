package com.sos.jobscheduler.data.workflow

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

  def moveTo(to: InstructionNr) = copy(nr = to)

  def /(childId: OrderId.ChildId) = new HalfPosition(this, childId)
  def /(id: String) = new HalfPosition(this, OrderId.ChildId(id))
  def /:(workflowPath: WorkflowPath) = new WorkflowPosition(workflowPath, this)

  def dropChild: Option[Position] =
    for (last ← parents.lastOption) yield
      Position(parents dropRight 1, last.nr)

  override def toString = "#" + (parents map (p ⇒ s"${p.nr.number}/${p.childId}/") mkString "") + nr.number

  def isNested = parents.nonEmpty

  def depth: Int = parents.size
}

object Position {
  implicit def apply(nr: InstructionNr): Position =
    Position(Nil, nr)

  implicit def apply(nr: Int): Position =
    Position(Nil, nr)

  implicit def apply(parentNr: Int, childId: String, nr: Int): Position =
    Position(Parent(parentNr, OrderId.ChildId(childId)) :: Nil, nr)

  final case class Parent(nr: InstructionNr, childId: OrderId.ChildId)

  final class HalfPosition private[Position](position: Position, childId: OrderId.ChildId) {
    def /(nr: InstructionNr) = Position(position.parents ::: Parent(position.nr, childId) :: Nil, nr)
  }

  implicit val jsonEncoder: Encoder[Position] =
    pos ⇒ Json.fromValues(toJsonSeq(pos))

  implicit val jsonDecoder: Decoder[Position] = {
    def decodeParents(pairs: Iterator[Seq[Json]]): Decoder.Result[List[Parent]] = {
      var left: Option[Left[DecodingFailure, Nothing]] = None
      val b = mutable.Buffer[Parent]()
      val parentResults: Iterator[Decoder.Result[Parent]] = pairs map decodeParent
      parentResults foreach {
        case Left(error) ⇒ left = Some(Left(error))
        case Right(parent) ⇒ b += parent
      }
      left getOrElse Right(b.toList)
    }

    def decodeParent(pair: Seq[Json]): Decoder.Result[Parent] =
      for {
        nr ← pair(0).as[InstructionNr]
        childId ← pair(1).as[OrderId.ChildId]
      } yield Parent(nr, childId)

    _.as[Seq[Json]] flatMap (parts ⇒
      if (parts.size % 2 != 1)
        Left(DecodingFailure("Not a valid Position", Nil))
      else
        for {
          parents ← decodeParents(parts dropRight 1 grouped 2)
          nr ← parts.last.as[InstructionNr]
        } yield Position(parents, nr))
  }

  private[workflow] def toJsonSeq(position: Position): Seq[Json] =
    position.parents.toVector.flatMap(p ⇒ Array(p.nr.asJson, p.childId.asJson)) :+ position.nr.asJson
}
