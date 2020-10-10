package js7.data.workflow.instructions

import io.circe.generic.JsonCodec
import js7.data.source.SourcePos
import js7.data.workflow.Instruction

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
object ImplicitEnd
{
  val empty = new ImplicitEnd()

  def apply(sourcePos: Option[SourcePos] = None) =
    sourcePos.fold(empty)(_ => new ImplicitEnd(sourcePos))
}
