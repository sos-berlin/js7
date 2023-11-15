package js7.provider

import java.util.concurrent.CancellationException
import js7.base.BuildInfo
import js7.base.configutils.Configs.logConfig
import js7.base.log.Logger
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.problem.Checked.Ops
import js7.base.system.startup.StartUp.logJavaSettings
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.common.system.startup.JavaMain.{runMain, withShutdownHooks}
import js7.provider.configuration.ProviderConfiguration
import monix.eval.Task
import monix.execution.CancelableFuture
import monix.execution.Scheduler.Implicits.traced
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
object ProviderMain
{
  coupleScribeWithSlf4j()

  private lazy val logger = Logger[this.type]

  def main(args: Array[String]): Unit =
    runMain("JS7 Provider") {
      // Log early for early timestamp and proper logger initialization by a single (not-parallel) call
      logger.info(s"Provider ${BuildInfo.prettyVersion}")
      val conf = ProviderConfiguration.fromCommandLine(args.toVector)
      logger.info(s"config=${conf.configDirectory}")
      logConfig(conf.config)
      logJavaSettings()
      val stop = Promise[Unit]()
      val terminated = Provider.observe(Task.fromFuture(stop.future), conf)
        .orThrow
        .completedL
        .runToFuture
      withShutdownHooks(conf.config, "ProviderMain", () => onJavaShutdown(stop, terminated)) {
        awaitTermination(terminated)
      }
    }

  private def onJavaShutdown(stop: Promise[Unit], future: CancelableFuture[Unit]): Unit = {
    logger.warn("Trying to terminate Provider due to Java shutdown")
    stop.trySuccess(())
    awaitTermination(future)
  }

  private def awaitTermination(future: Future[Unit]): Unit = {
    future.recover { case _: CancellationException => }.awaitInfinite
    logger.info("JS7 Provider terminated")
  }
}
