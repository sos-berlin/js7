package js7.data.workflow.instructions

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.data.source.SourcePos
import js7.data.value.expression.Expression
import js7.data.workflow.Instruction

final case class Sleep(duration: Expression, sourcePos: Option[SourcePos] = None)
extends Instruction.NoInstructionBlock:

  def withoutSourcePos: Sleep =
    copy(sourcePos = None)

  override def toString: String =
    s"sleep $duration$sourcePosToString"


object Sleep:
  given Codec.AsObject[Sleep] = deriveCodec[Sleep]
