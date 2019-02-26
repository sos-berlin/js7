package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.workflow.Instruction
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
sealed case class Fail(returnCode: Option[ReturnCode]) extends Instruction
{
  override def toString = "fail"
}

object Fail extends Fail(None)
