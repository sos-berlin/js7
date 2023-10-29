package js7.base.log

import java.lang.reflect.Method
import java.time.LocalDateTime
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.Once
import monix.execution.atomic.AtomicBoolean
import scala.concurrent.duration.Deadline
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
object Log4j:

  private val isShutdown = AtomicBoolean(false)
  private val startedAt = Timestamp.now
  private val runningSince = Deadline.now
  private val ifNotInitialized = new Once

  // Do not touch the logger before initialize has been called !!!
  private lazy val logger = Logger[this.type]

  private lazy val shutdownMethod: Try[Method] =
    Try(Class.forName("org.apache.logging.log4j.LogManager"))
      .flatMap(_
        .getMethod("shutdown", classOf[Boolean], classOf[Boolean]) match {
          case null => Failure(new RuntimeException("Missing method org.apache.logging.log4j.LogManager(Boolean, Boolean)"))
          case o => Success(o)
        })

  def initialize(name: String) =
    ifNotInitialized:
      if CorrelId.couldBeEnabled then CorrelIdLog4jThreadContextMap.initialize(name)
      for t <- shutdownMethod.failed do logger.warn(t.toString)

  def setDefaultConfiguration(resource: String): Unit =
    if !sys.props.contains("log4j.configurationFile") then
      val uri = s"classpath:$resource"
      System.setProperty("log4j.configurationFile", uri)
      logger.debug(s"Default log4j.configurationFile=$uri")

  /**
    * Call in case the shutdown hook is disabled in log4j2.xml: &gt;configuration shutdownHook="disable">.
    */
  def shutdown(fast: Boolean = false): Unit =
    if !isShutdown.getAndSet(true) then
      if !fast then
        CorrelId.logStatisticsIfEnabled()
        CorrelIdLog4jThreadContextMap.logStatistics()
      for shutdown <- shutdownMethod do
        // Log complete timestamp in case of short log timestamp
        logger.info("shutdown at " +
          LocalDateTime.now.toString.replace('T', ' ') +
          ", started at " + startedAt.roundDownTo(1.s).pretty +
          " (" + runningSince.elapsed.pretty + " ago)" + "\n" +
          "┄" * 80 + "\n")
        shutdown.invoke(null, false, false)
