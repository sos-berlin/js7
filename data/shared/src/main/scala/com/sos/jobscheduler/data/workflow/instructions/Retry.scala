package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.data.workflow.Instruction

/**
  * @author Joacim Zschimmer
  */
sealed trait Retry extends Instruction

case object Retry extends Retry
{
  override def toString = "retry"
}
