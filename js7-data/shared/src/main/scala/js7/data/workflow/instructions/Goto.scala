package js7.data.workflow.instructions

import io.circe.generic.semiauto.deriveCodec
import js7.data.source.SourcePos
import js7.data.workflow.{JumpInstruction, Label}

/**
  * @author Joacim Zschimmer
  */
final case class Goto(to: Label, sourcePos: Option[SourcePos] = None) extends JumpInstruction
{
  def withoutSourcePos = copy(sourcePos = None)

  override def toString = s"goto $to$sourcePosToString"
}

object Goto {
  implicit val jsonCodec = deriveCodec[Goto]
}
