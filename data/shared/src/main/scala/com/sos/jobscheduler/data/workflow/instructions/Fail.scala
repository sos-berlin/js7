package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.Instruction
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
sealed case class Fail(
  returnCode: Option[ReturnCode] = None,
  sourcePos: Option[SourcePos] = None) extends Instruction
{
  def withoutSourcePos = copy(sourcePos = None)

  override def toString = "fail"
}
