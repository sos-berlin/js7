package js7.data.workflow.instructions

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.data.source.SourcePos
import js7.data.workflow.Instruction

/**
  * @author Joacim Zschimmer
  */
sealed trait End extends Instruction.NoInstructionBlock

final case class ExplicitEnd(sourcePos: Option[SourcePos] = None) extends End:

  def withoutSourcePos: ExplicitEnd =
    copy(sourcePos = None)

  override def toString =
    "end" + sourcePosToString


object ExplicitEnd:
  implicit val jsonCodec: Codec.AsObject[ExplicitEnd] = deriveCodec


final case class ImplicitEnd(sourcePos: Option[SourcePos] = None) extends End:
  def withoutSourcePos: ImplicitEnd =
    copy(sourcePos = None)

  override def toString =
    "end/*implicit*/" + sourcePosToString


object ImplicitEnd:
  val empty = new ImplicitEnd()
  implicit val jsonCodec: Codec.AsObject[ImplicitEnd] = deriveCodec

  def apply(sourcePos: Option[SourcePos] = None): ImplicitEnd =
    sourcePos.fold(empty)(_ => new ImplicitEnd(sourcePos))
