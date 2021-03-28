package js7.data.workflow.instructions

import io.circe.generic.semiauto.deriveCodec
import js7.data.source.SourcePos
import js7.data.workflow.{JumpInstruction, Label}

/**
  * @author Joacim Zschimmer
  */
final case class IfFailedGoto(to: Label, sourcePos: Option[SourcePos] = None) extends JumpInstruction
{
  def withoutSourcePos = copy(sourcePos = None)

  override def toString = s"ifFailedGoto $to$sourcePosToString"
}

object IfFailedGoto {
  implicit val jsonCodec = deriveCodec[IfFailedGoto]
}
