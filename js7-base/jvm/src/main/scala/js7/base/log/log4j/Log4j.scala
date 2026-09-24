package js7.base.log.log4j

import java.lang.reflect.Method
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax.ifFailed
import js7.base.utils.{Atomic, Once}
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
object Log4j:

  /** 32768 should reduce memory usage with 40MB or 50MB.
    *
    * Default is 256 * 1024. */
  private val asyncLoggerRingBufferSize: Int = 32768
  private val isShutdown = Atomic(false)
  private val ifNotInitialized = new Once
  private var earlyInitialized = false

  // Do not touch the logger before initialize has been called !!!
  private lazy val logger = Logger[this.type]

  private lazy val shutdownMethod: Try[Method] =
    Try(Class.forName("org.apache.logging.log4j.LogManager"))
      .flatMap:
        _.getMethod("shutdown", classOf[Boolean], classOf[Boolean]) match
          case null => Failure(RuntimeException:
            "Missing method org.apache.logging.log4j.LogManager(Boolean, Boolean)")
          case o => Success(o)

  private def isInitialized: Boolean =
    ifNotInitialized.isInitialized

  def earlyInitializeForProduction(): Unit =
    useAsyncLogger()
    if isInitialized && !earlyInitialized then
      logger.error("earlyInitializeForProduction but Log4j has already been initialized",
        new Exception)
    earlyInitialized = true

  private def useAsyncLogger(): Unit =
    sys.props("log4j2.contextSelector") =
      classOf[org.apache.logging.log4j.core.async.AsyncLoggerContextSelector].getName

    if !sys.props.contains("log4j2.asyncLoggerRingBufferSize")
      && !sys.env.contains("LOG4J_ASYNC_LOGGER_RING_BUFFER_SIZE")
    then
      // https://logging.apache.org/log4j/2.x/manual/async.html#log4j2.asyncLoggerRingBufferSize
      sys.props("log4j2.asyncLoggerRingBufferSize") = asyncLoggerRingBufferSize.toString

    if !sys.props.contains("log4j2.asyncLoggerWaitStrategy")
      && !sys.env.contains("LOG4J_ASYNC_LOGGER_WAIT_STRATEGY")
    then
      // When idling, "Block" uses less CPU than default "Timeout"
      // https://logging.apache.org/log4j/2.x/manual/async.html#log4j2.asyncLoggerWaitStrategy
      sys.props("log4j2.asyncLoggerWaitStrategy") = "Block"

    // Because AsyncLoggerContextSelector already flushes:
    sys.props("js7.log4j.immediateFlush") = "false"

  def initialize(name: String): Unit =
    ifNotInitialized:
      Log4jThreadContextMap.initialize(name)
      for t <- shutdownMethod.ifFailed do logger.warn(t.toString)

  /**
    * Call in case the shutdown hook is disabled in log4j2.xml: &lt;configuration shutdownHook="disable">.
    */
  def shutdown(fast: Boolean = false, suppressLogging: Boolean = false): Unit =
    // Don't log here, because Logger.shutdown as already written the terminating "STOP" line
    if !isShutdown.getAndSet(true) then
      for shutdown <- shutdownMethod do
        shutdown.invoke(null, false, false)

  /** Set a key-value pair accessible in the log4j2 configuration via %X{key}. */
  def putGlobal(key: String, value: String) =
    Log4jThreadContextMap.put(key, value)
