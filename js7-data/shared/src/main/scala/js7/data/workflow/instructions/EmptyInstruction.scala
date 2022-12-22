package js7.data.workflow.instructions

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.data.source.SourcePos
import js7.data.workflow.Instruction

final case class EmptyInstruction(sourcePos: Option[SourcePos])
extends Instruction
{
  def withoutSourcePos = copy(sourcePos = None)

  override def toString = "gap"
}

object EmptyInstruction
{
  val empty = new EmptyInstruction(None)

  def apply(sourcePos: Option[SourcePos] = None): EmptyInstruction =
    sourcePos.fold(empty)(_ => new EmptyInstruction(sourcePos))

  implicit val jsonCodec: Codec.AsObject[EmptyInstruction] = deriveCodec
}
