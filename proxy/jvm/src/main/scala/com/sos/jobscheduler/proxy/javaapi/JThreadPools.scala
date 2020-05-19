package com.sos.jobscheduler.proxy.javaapi

import com.sos.jobscheduler.base.annotation.javaApi
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.scalautil.Logger
import java.lang.Thread.currentThread
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
