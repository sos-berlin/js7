package js7.data.workflow.instructions

import io.circe.generic.semiauto.deriveCodec
import js7.data.source.SourcePos
import js7.data.workflow.Instruction

/**
  * @author Joacim Zschimmer
  */
sealed trait End extends Instruction

final case class ExplicitEnd(sourcePos: Option[SourcePos] = None) extends End
{
  def withoutSourcePos = copy(sourcePos = None)

  override def toString = "end" + sourcePosToString
}

object ExplicitEnd {
  implicit val jsonCodec = deriveCodec[ExplicitEnd]
}

final case class ImplicitEnd(sourcePos: Option[SourcePos] = None) extends End
{
  def withoutSourcePos = copy(sourcePos = None)

  override def toString = "end/*implicit*/" + sourcePosToString
}
object ImplicitEnd
{
  val empty = new ImplicitEnd()
  implicit val jsonCodec = deriveCodec[ImplicitEnd]

  def apply(sourcePos: Option[SourcePos] = None) =
    sourcePos.fold(empty)(_ => new ImplicitEnd(sourcePos))
}
