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

  private val isShutdown = Atomic(false)
  private val ifNotInitialized = new Once

  private val RingBufferFractionOfHeap = 3 / 100.0
  private val RingBufferMax = 4096 // Log4j's default for non-webapp, gc-free mode
  private val RingBufferEstimatedEntrySize = 744

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

  /** Call this only when JS7 Engine is not user as a library in a surrounding application! */
  private[log] def initialize(name: String): Unit =
    ifNotInitialized:
      if sys.runtime.availableProcessors > 1 then
        // Fast AsyncLogger requires Heap for the RingBuffer and a little more CPU time
        useAsyncLogger()
      end if

      // Maybe not required ...
      //if !sys.props.contains("log4j2.isWebapp")
      //  && !sys.env.contains("LOG4J2_IS_WEBAPP")
      //  && !sys.props.contains("log4j2.enableThreadlocals")
      //  && !sys.env.contains("LOG4J2_ENABLE_THREADLOCALS")
      //then
      //  sys.props("log4j2.isWebapp") = "false"
      //  // Switch off gc-free mode and reduce memory usage
      //  //sys.props("log4j2.enableThreadlocals") = "false"

      Log4jThreadContextMap.initialize(name)
      for t <- shutdownMethod.ifFailed do logger.warn(t.toString)

  private def useAsyncLogger(): Unit =
    sys.props("log4j2.contextSelector") =
      classOf[org.apache.logging.log4j.core.async.AsyncLoggerContextSelector].getName

    if !sys.props.contains("log4j2.asyncLoggerRingBufferSize")
      && !sys.env.contains("LOG4J_ASYNC_LOGGER_RING_BUFFER_SIZE")
    then
      // https://logging.apache.org/log4j/2.x/manual/async.html#log4j2.asyncLoggerRingBufferSize
      sys.props("log4j2.asyncLoggerRingBufferSize") =
        heapSizeToRingBufferSize(sys.runtime.maxMemory()).toString

    if !sys.props.contains("log4j2.asyncLoggerWaitStrategy")
      && !sys.env.contains("LOG4J_ASYNC_LOGGER_WAIT_STRATEGY")
    then
      // When idling, "Block" uses less CPU than default "Timeout"
      // https://logging.apache.org/log4j/2.x/manual/async.html#log4j2.asyncLoggerWaitStrategy
      sys.props("log4j2.asyncLoggerWaitStrategy") = "Block"

    // Because AsyncLoggerContextSelector already flushes:
    sys.props("js7.log4j.immediateFlush") = "false"

  /** Log4j's default is 256 * 1024 (but 4096 in gc-free mode).
    *
    * Log4j seems to use 352 to 744 bytes per LMAX RingBuffer entry
    * (https://github.com/apache/logging-log4j2/issues/3569).
    *
    * So a value of 4096 should use not more than 3MB memory.
    */
  private[log4j] def heapSizeToRingBufferSize(maxHeap: Long) =
    Integer.highestOneBit:
      (maxHeap * RingBufferFractionOfHeap / RingBufferEstimatedEntrySize).toInt.max(1)
    .min(RingBufferMax)

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
