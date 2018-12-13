package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.workflow.Instruction
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class Fail(returnCode: Option[ReturnCode]) extends Instruction
{
  override def toString = "fail"
}
