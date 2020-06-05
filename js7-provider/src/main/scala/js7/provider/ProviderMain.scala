package js7.provider

import js7.base.BuildInfo
import js7.base.problem.Checked.Ops
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.scalautil.Futures.implicits.SuccessFuture
import js7.common.scalautil.IOExecutor.Implicits.globalIOX
import js7.common.scalautil.Logger
import js7.core.startup.JavaMain.{runMain, withShutdownHooks}
import js7.core.startup.StartUp.logStartUp
import js7.provider.configuration.ProviderConfiguration
import java.util.concurrent.CancellationException
import monix.execution.CancelableFuture
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
object ProviderMain
{
  coupleScribeWithSlf4j()

  private val logger = Logger(getClass)

  def main(args: Array[String]): Unit = {
    logger.info(s"Provider ${BuildInfo.prettyVersion}")  // Log early for early timestamp and proper logger initialization by a single (not-parallel) call
    runMain {
      val conf = ProviderConfiguration.fromCommandLine(args.toVector)
      logStartUp(configDir = conf.configDirectory, dataDir = None)
      val cancelable = Provider.observe(conf).orThrow.onCancelTriggerError foreach { _ => }
      withShutdownHooks(conf.config, "ProviderMain", onJavaShutdown(cancelable)) {
        awaitTermination(cancelable)
      }
    }
  }

  private def onJavaShutdown(cancelable: CancelableFuture[Unit])(timeout: Duration): Unit = {
    logger.warn("Trying to terminate Provider due to Java shutdown")
    cancelable.cancel()
    awaitTermination(cancelable)
  }

  private def awaitTermination(future: Future[Unit]): Unit = {
    future.recover { case _: CancellationException => }.awaitInfinite
    logger.info("JS7 Provider terminated")
  }
}
