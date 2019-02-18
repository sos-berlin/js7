package com.sos.jobscheduler.common.log

import com.sos.jobscheduler.common.scalautil.Logger
import monix.execution.atomic.AtomicBoolean
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
object Log4j {

  private val logger = Logger(getClass)
  private val isShutdown = AtomicBoolean(false)

  /**
    * Call in case the shutdown hook is disabled in log4j2.xml: &gt;configuration shutdownHook="disable">.
    */
  def shutdown(): Unit =
    if (!isShutdown.getAndSet(true)) {
      for (logManager ← Try(Class.forName("org.apache.logging.log4j.LogManager"))) {
        logger.debug("log4j.LogManager.shutdown")
        logManager.getMethod("shutdown", classOf[Boolean]).invoke(null, Boolean.box(true))
      }
    }
}
