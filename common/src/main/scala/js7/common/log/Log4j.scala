package js7.common.log

import js7.common.scalautil.Logger
import java.time.LocalDateTime
import monix.execution.atomic.AtomicBoolean
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
object Log4j
{
  private lazy val logger = Logger(getClass)
  private val isShutdown = AtomicBoolean(false)

  def setDefaultConfiguration(resource: String): Unit = {
    if (!sys.props.contains("log4j.configurationFile")) {
      val uri = s"classpath:$resource"
      System.setProperty("log4j.configurationFile", uri)
      logger.debug(s"Default log4j.configurationFile=$uri")
    }
  }

  /**
    * Call in case the shutdown hook is disabled in log4j2.xml: &gt;configuration shutdownHook="disable">.
    */
  def shutdown(): Unit =
    if (!isShutdown.getAndSet(true)) {
      for (logManager <- Try(Class.forName("org.apache.logging.log4j.LogManager"))) {
        // Log complete timestamp in case of short log timestamp
        logger.debug(s"log4j.LogManager.shutdown at ${LocalDateTime.now.toString.replace('T', ' ')}")
        logManager.getMethod("shutdown", classOf[Boolean]).invoke(null, Boolean.box(true))
      }
    }
}
