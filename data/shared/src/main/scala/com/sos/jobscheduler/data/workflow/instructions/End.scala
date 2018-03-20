package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.data.workflow.Instruction

/**
  * @author Joacim Zschimmer
  */
sealed trait End extends Instruction

case object ExplicitEnd extends End {
  override def toString = "end"
}

case object ImplicitEnd extends End {
  override def toString = "end/*implicit*/"
}
