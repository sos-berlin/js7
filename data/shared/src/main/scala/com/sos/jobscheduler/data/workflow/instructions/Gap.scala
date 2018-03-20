package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.data.workflow.Instruction

/**
  * @author Joacim Zschimmer
  */
sealed trait Gap extends Instruction
/** reduceForAgent uses Gap for all instructions not executable on the requested Agent. */

case object Gap extends Gap
{
  override def toString = "gap"
}
