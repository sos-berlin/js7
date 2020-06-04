package com.sos.jobscheduler.core.system

import com.sos.jobscheduler.base.convert.As
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.startup.Halt.haltJava
import com.typesafe.config.Config
import java.lang.Thread.currentThread
import monix.execution.schedulers.ExecutorScheduler
import monix.execution.{ExecutionModel, UncaughtExceptionReporter}
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object ThreadPools
{
  private val logger = Logger(getClass)

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
        haltJava(s"HALT DUE TO $throwable", restart = true)

      case throwable =>
        logger.error(msg, throwable.nullIfNoStackTrace)
        // Writes to stderr:
        UncaughtExceptionReporter.default.reportFailure(throwable)
    }
  }

  def newStandardScheduler(name: String, config: Config): ExecutorScheduler =
    ExecutorScheduler.forkJoinDynamic(name,
      parallelism = config.as("jobscheduler.thread-pools.standard.parallelism")(ThreadCount),
      maxThreads = config.as("jobscheduler.thread-pools.standard.maximum")(ThreadCount),
      daemonic = true,
      reporter = uncaughtExceptionReporter,
      ExecutionModel.Default)
}
