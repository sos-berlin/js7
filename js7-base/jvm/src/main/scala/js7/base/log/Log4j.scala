package js7.base.log

import java.lang.reflect.Method
import java.time.LocalDateTime
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.ifFailed
import js7.base.utils.{Atomic, Once}
import scala.concurrent.duration.Deadline
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
object Log4j:

  private val isShutdown = Atomic(false)
  private val startedAt = Timestamp.now
  private val runningSince = Deadline.now
  private val ifNotInitialized = new Once
  private var earlyInitialized = false

  // Do not touch the logger before initialize has been called !!!
  private lazy val logger = Logger[this.type]

  private lazy val shutdownMethod: Try[Method] =
    Try(Class.forName("org.apache.logging.log4j.LogManager"))
      .flatMap(_
        .getMethod("shutdown", classOf[Boolean], classOf[Boolean]) match {
          case null => Failure(new RuntimeException("Missing method org.apache.logging.log4j.LogManager(Boolean, Boolean)"))
          case o => Success(o)
        })

  private def isInitialized: Boolean =
    ifNotInitialized.isInitialized

  def earlyInitializeForProduction(): Unit =
    useAsyncLogger()
    if isInitialized && !earlyInitialized then
      logger.error("earlyInitializeForProduction but Log4j has already been initialized")
    earlyInitialized = true

  private def useAsyncLogger(): Unit =
    sys.props("log4j2.contextSelector") =
      classOf[org.apache.logging.log4j.core.async.AsyncLoggerContextSelector].getName
    // Use less CPU when idling than default "Timeout":
    sys.props("log4j2.asyncLoggerWaitStrategy") = "Block"
    // Because AsyncLoggerContextSelector flushes:
    sys.props("js7.log4j.immediateFlush") = "false"

  def initialize(name: String): Unit =
    ifNotInitialized:
      Log4jThreadContextMap.initialize(name)
      for t <- shutdownMethod.ifFailed do logger.warn(t.toString)

  /**
    * Call in case the shutdown hook is disabled in log4j2.xml: &lt;configuration shutdownHook="disable">.
    */
  def shutdown(fast: Boolean = false, suppressLogging: Boolean = false): Unit =
    if !isShutdown.getAndSet(true) then
      if !fast then
        CorrelId.logStatisticsIfEnabled()
        Log4jThreadContextMap.logStatistics()
      for shutdown <- shutdownMethod do
        if !suppressLogging then
          // Log complete timestamp in case of short log timestamp
          logger.info("Shutdown at " +
            LocalDateTime.now.toString.replace('T', ' ') +
            " (started at " + startedAt.roundDownTo(1.s).pretty +
            " " + runningSince.elapsed.pretty + " ago)" + "\n" +
            "┄" * 80 + "\n")
        shutdown.invoke(null, false, false)

  /** Set variable accessible in the log4j2 configuration via %X{key}. */
  def set(key: String, value: String) =
    Log4jThreadContextMap.set(key, value)
