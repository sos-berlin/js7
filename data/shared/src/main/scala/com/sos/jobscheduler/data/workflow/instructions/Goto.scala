package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.{JumpInstruction, Label}
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class Goto(to: Label, sourcePos: Option[SourcePos] = None) extends JumpInstruction
{
  def withoutSourcePos = copy(sourcePos = None)

  override def toString = s"goto $to"
}
