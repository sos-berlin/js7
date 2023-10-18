package js7.data.workflow.instructions

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.data.source.SourcePos
import js7.data.value.expression.Expression
import js7.data.workflow.Instruction

final case class Prompt(question: Expression, sourcePos: Option[SourcePos] = None)
extends Instruction:
  def withoutSourcePos = copy(sourcePos = None)


object Prompt:
  implicit val jsonCodec: Codec.AsObject[Prompt] = deriveCodec[Prompt]
