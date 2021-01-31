package js7.data.workflow.instructions

import io.circe.generic.JsonCodec
import js7.data.source.SourcePos
import js7.data.workflow.Instruction

/** reduceForAgent uses Gap for all instructions not executable on the requested Agent.
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class Gap(sourcePos: Option[SourcePos])
extends Instruction
{
  def withoutSourcePos = copy(sourcePos = None)

  override def toString = "gap"
}

object Gap
{
  val empty = new Gap(None)

  def apply(sourcePos: Option[SourcePos] = None): Gap =
    sourcePos.fold(empty)(_ => new Gap(sourcePos))
}
