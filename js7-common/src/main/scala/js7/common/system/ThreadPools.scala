package js7.common.system

import com.typesafe.config.Config
import java.lang.Thread.currentThread
import js7.base.convert.As
import js7.base.time.ScalaTime._
import js7.base.utils.Closer
import js7.base.utils.ScalaUtils.syntax._
import js7.common.configutils.Configs.ConvertibleConfig
import js7.common.scalautil.Logger
import js7.common.system.startup.Halt.haltJava
import js7.common.system.startup.StartUp.printlnWithClockIgnoringException
import js7.common.time.JavaTimeConverters.AsScalaDuration
import js7.common.utils.ByteUnits.toKiBGiB
import monix.execution.schedulers.ExecutorScheduler
import monix.execution.{ExecutionModel, UncaughtExceptionReporter}
import scala.concurrent.duration.Duration
import scala.util.control.NonFatal
import scala.jdk.CollectionConverters._

/**
  * @author Joacim Zschimmer
  */
object ThreadPools
{
  private val logger = Logger(getClass)
  private val heapSizeMessage = s"Heap size is ${toKiBGiB(sys.runtime.maxMemory)}"

  private[system] val ThreadCount = As[String, Int] {
    case s if s.last == 'x' => (sys.runtime.availableProcessors * s.dropRight(1).toDouble).ceil.toInt
    case o => o.toInt
  }

  private val uncaughtExceptionReporter: UncaughtExceptionReporter = { throwable =>
    def msg = s"Uncaught exception in thread ${currentThread.getId} '${currentThread.getName}': ${throwable.toStringWithCauses}"
    throwable match {
      case throwable: akka.http.scaladsl.model.EntityStreamException =>
        // TODO Not sure how to handle or ignore an unexpectedly closed connection while reading a stream.
        // "Entity stream truncation. The HTTP parser was receiving an entity when the underlying connection was closed unexpectedly."
        logger.warn(msg, throwable.nullIfNoStackTrace)

      case NonFatal(_) =>
        logger.error(msg, throwable.nullIfNoStackTrace)

      case throwable: OutOfMemoryError =>
        logger.error(msg, throwable.nullIfNoStackTrace)
        // Writes to stderr:
        UncaughtExceptionReporter.default.reportFailure(throwable)
        logger.error(heapSizeMessage)
        printlnWithClockIgnoringException(heapSizeMessage)
        haltJava(s"HALT DUE TO $throwable", restart = true)

      case throwable =>
        logger.error(msg, throwable.nullIfNoStackTrace)
        // Writes to stderr:
        UncaughtExceptionReporter.default.reportFailure(throwable)
    }
  }

  def newStandardScheduler(name: String, config: Config, closer: Closer): ExecutorScheduler = {
    val shutdownTimeout = config.getDuration("js7.thread-pools.standard.shutdown-timeout").toFiniteDuration
    val scheduler = ExecutorScheduler.forkJoinDynamic(name,
      parallelism = config.as("js7.thread-pools.standard.parallelism")(ThreadCount),
      maxThreads = config.as("js7.thread-pools.standard.maximum")(ThreadCount),
      daemonic = true,
      reporter = uncaughtExceptionReporter,
      ExecutionModel.Default)

    closer.onClose {
      logger.debug("shutdown")
      scheduler.shutdown()
      if (shutdownTimeout > Duration.Zero) {
        logger.debug(s"awaitTermination(${shutdownTimeout.pretty}) ...")
        if (!scheduler.awaitTermination(shutdownTimeout)) {
          logger.debug(s"awaitTermination(${shutdownTimeout.pretty}) timed out" +
            Thread.getAllStackTraces.asScala
              .filter(_._1.getName startsWith name)
              .toSeq.sortBy(_._1.getId)
              .map { case (thread, stacktrace) => s"\nThread #${ thread.getId } ${thread.getName}:" + stacktrace.map(o => s"\n  $o").mkString }
              .mkString)
        } else {
          logger.debug(s"awaitTermination() finished")
        }
      }
    }

    scheduler
  }
}
