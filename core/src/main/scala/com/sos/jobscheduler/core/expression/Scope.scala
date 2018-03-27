package com.sos.jobscheduler.core.expression

import com.sos.jobscheduler.data.job.ReturnCode

/**
  * @author Joacim Zschimmer
  */
final class Scope(
  val returnCode: ReturnCode,
  val variableNameToString: PartialFunction[String, String])
