package js7.data.workflow.instructions

import js7.data.source.SourcePos
import js7.data.workflow.{JumpInstruction, Label}
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class IfFailedGoto(to: Label, sourcePos: Option[SourcePos] = None) extends JumpInstruction
{
  def withoutSourcePos = copy(sourcePos = None)

  override def toString = s"ifFailedGoto $to"
}
