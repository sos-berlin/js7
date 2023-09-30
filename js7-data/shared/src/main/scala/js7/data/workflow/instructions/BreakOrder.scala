package js7.data.workflow.instructions

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.data.source.SourcePos
import js7.data.workflow.Instruction

/** For internal JS7 testing only. */
final case class BreakOrder(sourcePos: Option[SourcePos] = None)
extends Instruction:

  def withoutSourcePos = copy(sourcePos = None)

  override def toString = "BreakOrder"

object BreakOrder:
  implicit val jsonCodec: Codec.AsObject[BreakOrder] = deriveCodec
