package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.Instruction
import io.circe.generic.JsonCodec

/** reduceForAgent uses Gap for all instructions not executable on the requested Agent.
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class Gap(sourcePos: Option[SourcePos] = None)
extends Instruction
{
  def withoutSourcePos = copy(sourcePos = None)

  override def toString = "gap"
}
