package com.sos.jobscheduler.core.startup

import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.Logger

/**
  * @author Joacim Zschimmer
  */
object Halt
{
  private val logger = Logger(getClass)

  def haltJava(msg: String, exitCode: Int = 99): Nothing = {
    logger.error(msg)
    Log4j.shutdown()
    sys.runtime.halt(exitCode)
    throw new Error("sys.runtime.halt failed")
  }
}
