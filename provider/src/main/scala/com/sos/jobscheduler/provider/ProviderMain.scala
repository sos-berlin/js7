package com.sos.jobscheduler.provider

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.IOExecutor.Implicits.globalIOX
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.JavaMainSupport.{runMain, withShutdownHooks}
import com.sos.jobscheduler.core.StartUp.logStartUp
import com.sos.jobscheduler.provider.configuration.ProviderConfiguration
import monix.execution.Cancelable
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
object ProviderMain
{
  private val logger = Logger(getClass)

  def main(args: Array[String]): Unit = {
    logger.info(s"Provider ${BuildInfo.prettyVersion}")  // Log early for early timestamp and proper logger initialization by a single (not-parallel) call
    runMain {
      val conf = ProviderConfiguration.fromCommandLine(args.toVector)
      logStartUp(configDir = conf.configDirectory, dataDir = None)
      val cancelable = Provider.observe(conf).orThrow foreach { _ ⇒ }
      withShutdownHooks(conf.config, "ProviderMain", onJavaShutdown(cancelable)) {
        cancelable.awaitInfinite
      }
    }
  }

  private def onJavaShutdown(cancelable: Cancelable)(timeout: Duration): Unit = {
    logger.warn("Trying to terminate Provider due to Java shutdown")
    cancelable.cancel()
  }
}
