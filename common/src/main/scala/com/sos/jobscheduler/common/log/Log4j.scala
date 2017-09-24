package com.sos.jobscheduler.common.log

import com.sos.jobscheduler.common.scalautil.Logger

/**
  * @author Joacim Zschimmer
  */
object Log4j {

  private val logger = Logger(getClass)

  /**
    * Call in case the shutdown hook is disabled in log4j2.xml: &gt;configuration shutdownHook="disable">.
    */
  def shutdown(): Unit = {
    for (logManager ← Option(Class forName "org.apache.logging.log4j.LogManager")) {
      logger.debug("log4j.LogManager.shutdown")
      logManager.getMethod("shutdown", classOf[Boolean]).invoke(null, Boolean.box(true))
    }
  }
}
