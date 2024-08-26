package js7.data.workflow.instructions

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.data.source.SourcePos
import js7.data.workflow.Instruction

final case class Stop(sourcePos: Option[SourcePos] = None)
extends Instruction.NoInstructionBlock:
  
  def withoutSourcePos: Stop =
    copy(sourcePos = None)

  override def toString = "Stop"


object Stop:
  implicit val jsonCodec: Codec.AsObject[Stop] = deriveCodec
