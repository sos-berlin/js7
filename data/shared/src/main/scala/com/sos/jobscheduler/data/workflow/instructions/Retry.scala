package js7.data.workflow.instructions

import js7.data.source.SourcePos
import js7.data.workflow.Instruction
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class Retry(sourcePos: Option[SourcePos] = None)
extends Instruction
{
  def withoutSourcePos = copy(sourcePos = None)

  override def toString = "retry"
}
