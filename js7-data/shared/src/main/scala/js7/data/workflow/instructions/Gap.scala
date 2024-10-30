package js7.data.workflow.instructions

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.data.source.SourcePos
import js7.data.workflow.Instruction

/** reduceForAgent uses Gap for all instructions not executable on the requested Agent.
  * @author Joacim Zschimmer
  */
final case class Gap(sourcePos: Option[SourcePos])
extends Instruction.NoInstructionBlock:

  def withoutSourcePos: Gap =
    copy(sourcePos = None)

  override def toString = "gap"


object Gap:
  val empty = new Gap(None)

  def apply(sourcePos: Option[SourcePos] = None): Gap =
    sourcePos.fold(empty)(_ => new Gap(sourcePos))

  implicit val jsonCodec: Codec.AsObject[Gap] = deriveCodec
