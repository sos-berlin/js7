package js7.data.workflow.instructions

import io.circe.generic.semiauto.deriveCodec
import io.circe.{Codec, Decoder, Encoder}
import js7.data.source.SourcePos
import js7.data.value.expression.Expression
import js7.data.workflow.Instruction

// Could be extended to send something (JSON?) via a kind of named channel connected
// with the outer world.
/** TEST ONLY — EXPERIMENTAL */
final case class Say(
  what: Expression,
  sourcePos: Option[SourcePos] = None)
extends
  Instruction.NoInstructionBlock:

  def withoutSourcePos: Say = copy(
    sourcePos = None)


object Say:
  given Codec.AsObject[Say] = deriveCodec
