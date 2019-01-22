package com.sos.jobscheduler.core.system

import com.sos.jobscheduler.base.convert.As
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.scalautil.Logger
import com.typesafe.config.Config
import monix.execution.schedulers.ExecutorScheduler
import monix.execution.{ExecutionModel, Scheduler, UncaughtExceptionReporter}
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object ThreadPools
{
  private val logger = Logger(getClass)

  private[system] val ThreadCount = As[String, Int] {
    case s if s.last == 'x' ⇒ (sys.runtime.availableProcessors * s.dropRight(1).toDouble).ceil.toInt
    case o ⇒ o.toInt
  }

  private val uncaughtExceptionReporter: UncaughtExceptionReporter = {
    case NonFatal(throwable) ⇒
      logger.error(s"Uncaught exception in thread '${Thread.currentThread.getName}': ${throwable.toStringWithCauses}", throwable)
    case throwable ⇒
      UncaughtExceptionReporter.default.reportFailure(throwable)
  }

  def newStandardScheduler(name: String, config: Config): Scheduler =
    ExecutorScheduler.forkJoinDynamic(name,
      parallelism = config.as("jobscheduler.thread-pools.standard.parallelism")(ThreadCount),
      maxThreads = config.as("jobscheduler.thread-pools.standard.maximum")(ThreadCount),
      daemonic = true,
      reporter = uncaughtExceptionReporter,
      ExecutionModel.Default)
}
