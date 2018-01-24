package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.utils.Strings.TruncatedString
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.OrderActorEvent
import com.sos.jobscheduler.data.workflow.Instruction.Labeled
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import scala.collection.immutable.Seq
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
trait Instruction
{
  def toShortString = toString truncateWithEllipsis 40

  final def @:(labels: Seq[Label]) = Labeled(labels, this)
  final def @:(label: Label) = Labeled(label :: Nil, this)
  final def @:(label: String) = Labeled(Label(label) :: Nil, this)
  final def @:(unit: Unit) = Labeled(Nil, this)
}

trait EventInstruction extends Instruction {
  def toEvent(order: Order[Order.State], context: OrderContext): Option[KeyedEvent[OrderActorEvent]]
}

trait PositionInstruction extends Instruction {
  def nextPosition(order: Order[Order.Processed], context: OrderContext): Option[Position]
}

trait JumpInstruction extends Instruction {
  def to: Label
}

object Instruction {
  val @: = Labeled

  /** Only for tests and application. */
  object simplify {
    implicit def fromInstruction(instruction: Instruction): Labeled =
      Labeled(Nil, instruction)
  }

  final case class Labeled(labels: Seq[Label], instruction: Instruction) {
    def toShortString = s"$labelsString ${instruction.toShortString}"

    override def toString = s"$labelsString $instruction"

    def labelsString = labels.map(o ⇒ s"$o:").mkString(" ")
  }
  object Labeled {
    implicit def jsonEncoder(implicit instrEncoder: Encoder[Instruction]): Encoder[Labeled] = {
      case Labeled(Seq(), instruction) ⇒
        instruction.asJson
      case Labeled(labels, instruction) ⇒
        Json.fromJsonObject(("labels" → labels.asJson) +: instruction.asJson.asObject.get)
    }

    implicit def jsonDecoder(implicit instrDecoder: Decoder[Instruction]): Decoder[Labeled] =
      cursor ⇒ for {
        instruction ← cursor.as[Instruction]
        labels ← cursor.get[Json]("labels") match {
          case Right(json) ⇒ json.as[Seq[Label]]
          case Left(_) ⇒ Right(Nil)
        }
      } yield Labeled(labels, instruction)
  }
}
