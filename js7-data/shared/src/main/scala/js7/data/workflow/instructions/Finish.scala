package js7.data.workflow.instructions

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.data.source.SourcePos
import js7.data.workflow.Instruction

/**
  * @author Joacim Zschimmer
  */
final case class Finish(sourcePos: Option[SourcePos] = None)
extends Instruction
{
  def withoutSourcePos = copy(sourcePos = None)
}

object Finish {
  implicit val jsonCodec: Codec.AsObject[Finish] = deriveCodec
}
