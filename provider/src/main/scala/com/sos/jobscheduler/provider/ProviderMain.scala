package com.sos.jobscheduler.provider

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.IOExecutor.Implicits.globalIOX
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.JavaMainSupport.{handleJavaShutdown, runMain}
import com.sos.jobscheduler.core.StartUp
import com.sos.jobscheduler.provider.configuration.ProviderConfiguration
import monix.execution.Scheduler.Implicits.global
import monix.execution.{Cancelable, Scheduler}
import scala.concurrent.duration.Duration

/**
  * @author Joacim Zschimmer
  */
object ProviderMain
{
  private val logger = Logger(getClass)

  def main(args: Array[String]): Unit =
    runMain {
      val conf = ProviderConfiguration.fromCommandLine(args.toVector)
      StartUp.logStartUp(configDir = conf.configDirectory, dataDir = None)
      val cancelable = Provider.observe(conf).orThrow foreach { _ ⇒ }
      handleJavaShutdown(conf.config, "ProviderMain", onJavaShutdown(cancelable)) {
        // ?
      }
      cancelable.awaitInfinite
    }

  private def onJavaShutdown(cancelable: Cancelable)(timeout: Duration)(implicit s: Scheduler): Unit = {
    logger.warn("Trying to terminate Provider due to Java shutdown")
    cancelable.cancel()
  }
}
