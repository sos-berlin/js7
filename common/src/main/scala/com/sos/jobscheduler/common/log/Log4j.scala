package com.sos.jobscheduler.common.log

/**
  * @author Joacim Zschimmer
  */
object Log4j {

  /**
    * Call in case the shutdown hook is disabled in log4j2.xml: &gt;configuration shutdownHook="disable">.
    */
  def shutdown(): Unit = {
    for (logManager ← Option(Class.forName("org.apache.logging.log4j.LogManager"))) {
      logManager.getMethod("shutdown").invoke(null)
    }
  }
}
