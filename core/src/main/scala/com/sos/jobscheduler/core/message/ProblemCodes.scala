package com.sos.jobscheduler.core.message

import com.sos.jobscheduler.base.problem.ProblemCode

/**
  * @author Joacim Zschimmer
  */
object ProblemCodes {
  val UnknownOrder = ProblemCode("UnknownOrder")
  private[message] val ForTesting = ProblemCode("ForTesting")
}
