package js7.data.workflow.instructions

import js7.data.source.SourcePos
import js7.data.workflow.Instruction
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
sealed trait End extends Instruction

@JsonCodec
final case class ExplicitEnd(sourcePos: Option[SourcePos] = None) extends End
{
  def withoutSourcePos = copy(sourcePos = None)

  override def toString = "end"
}

@JsonCodec
final case class ImplicitEnd(sourcePos: Option[SourcePos] = None) extends End
{
  def withoutSourcePos = copy(sourcePos = None)

  override def toString = "end/*implicit*/"
}
