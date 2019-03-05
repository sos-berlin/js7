package com.sos.jobscheduler.core.expression

import com.sos.jobscheduler.data.job.ReturnCode

/**
  * @author Joacim Zschimmer
  */
trait Scope {
  val returnCode: ReturnCode
  val catchCount: Int
  val variableNameToString: PartialFunction[String, String]
}
