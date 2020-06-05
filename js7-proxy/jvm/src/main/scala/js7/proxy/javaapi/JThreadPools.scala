package js7.proxy.javaapi

import java.lang.Thread.currentThread
import js7.base.annotation.javaApi
import js7.base.utils.ScalaUtils.RichThrowable
import js7.common.scalautil.Logger
import monix.execution.schedulers.ExecutorScheduler
import monix.execution.{ExecutionModel, UncaughtExceptionReporter}
import scala.util.control.NonFatal

@javaApi
private[javaapi] object JThreadPools
{
  private val logger = Logger(getClass)

  private val uncaughtExceptionReporter: UncaughtExceptionReporter = { throwable =>
    logger.error(
      s"Uncaught exception in thread ${currentThread.getId} '${currentThread.getName}': ${throwable.toStringWithCauses}",
      throwable.nullIfNoStackTrace)
    throwable match {
      case NonFatal(_) =>
      case throwable =>
        // Writes to stderr:
        UncaughtExceptionReporter.default.reportFailure(throwable)
    }
  }

  def newStandardScheduler(parallelism: Int, maxThreads: Int, name: String): ExecutorScheduler =
    ExecutorScheduler.forkJoinDynamic(
      name,
      parallelism = parallelism,
      maxThreads = maxThreads,
      daemonic = true,
      reporter = uncaughtExceptionReporter,
      ExecutionModel.Default)
}
