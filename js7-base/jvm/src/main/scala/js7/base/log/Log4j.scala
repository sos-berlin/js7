package js7.base.log

import java.lang.reflect.Method
import java.time.LocalDateTime
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import monix.execution.atomic.AtomicBoolean
import scala.concurrent.duration.Deadline
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
object Log4j
{
  private val isShutdown = AtomicBoolean(false)
  private val startedAt = Timestamp.now
  private val runningSince = Deadline.now
  private var initialized = false

  // Do not touch the logger before initialize has been called !!!
  private lazy val logger = Logger[this.type]

  private lazy val shutdownMethod: Try[Method] =
    Try(Class.forName("org.apache.logging.log4j.LogManager"))
      .flatMap(_
        .getMethod("shutdown", classOf[Boolean], classOf[Boolean]) match {
          case null => Failure(new RuntimeException("Missing method org.apache.logging.log4j.LogManager(Boolean, Boolean)"))
          case o => Success(o)
        })

  def initialize() =
    synchronized {
      if (!initialized) {
        if (CorrelId.couldBeEnabled) CorrelIdLog4JThreadContextMap.initialize()
        for (t <- shutdownMethod.failed) logger.warn(t.toString)
        initialized = true
      }
    }

  def setDefaultConfiguration(resource: String): Unit =
    if (!sys.props.contains("log4j.configurationFile")) {
      val uri = s"classpath:$resource"
      System.setProperty("log4j.configurationFile", uri)
      logger.debug(s"Default log4j.configurationFile=$uri")
    }

  /**
    * Call in case the shutdown hook is disabled in log4j2.xml: &gt;configuration shutdownHook="disable">.
    */
  def shutdown(): Unit =
    if (!isShutdown.getAndSet(true)) {
      CorrelId.logStatisticsIfEnabled()
      CorrelIdLog4JThreadContextMap.logStatistics()
      for (shutdown <- shutdownMethod) {
        // Log complete timestamp in case of short log timestamp
        logger.info("shutdown at " +
          LocalDateTime.now.toString.replace('T', ' ') +
          ", started at " + startedAt.roundDownTo(1.s).pretty +
          " (" + runningSince.elapsed.pretty + " ago)" + "\n" +
          "┄" * 80 + "\n")
        shutdown.invoke(null, false, false)
      }
    }
}
