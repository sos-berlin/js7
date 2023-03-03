package js7.controller

import cats.effect.SyncIO
import js7.base.BuildInfo
import js7.base.configutils.Configs.logConfig
import js7.base.log.Logger
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.base.utils.SyncResource.syntax.RichSyncResource
import js7.common.commandline.CommandLineArguments
import js7.common.system.startup.JavaMain.withShutdownHooks
import js7.common.system.startup.JavaMainLockfileSupport.lockAndRunMain
import js7.common.system.startup.StartUp.{logJavaSettings, nowString, printlnWithClock, startUpLine}
import js7.common.system.startup.{Js7ReturnCodes, StartUp}
import js7.controller.configuration.ControllerConfiguration
import monix.execution.Scheduler
import scala.concurrent.duration.{Deadline, Duration, NANOSECONDS}

/**
  * JS7 Controller.
  *
  * @author Joacim Zschimmer
  */
final class ControllerMain
{
  private val logger = Logger(getClass)

  def run(arguments: CommandLineArguments): ProgramTermination = {
    // Log early for early timestamp and proper logger initialization by a
    // single (non-concurrent) call
    // Log a bar, in case the previous file is appended
    logger.info("JS7 Controller " + BuildInfo.longVersion +
      "\n" + "━" * 80)
    logger.info(startUpLine())
    logger.debug(arguments.toString)
    val conf = ControllerConfiguration.fromCommandLine(arguments)
    logger.info(s"${conf.controllerId} config=${conf.configDirectory} data=${conf.dataDirectory}")
    logConfig(conf.config)
    logJavaSettings()

    val termination =
      RunningController.threadPoolResource[SyncIO](conf).useSync { implicit scheduler =>
        val (controller, stop) = RunningController.resource(conf, scheduler).allocated.awaitInfinite
        withShutdownHooks(conf.config, "ControllerMain", () => onJavaShutdown(controller)) {
          val termination = controller.terminated.awaitInfinite
          stop.awaitInfinite
          termination
        }
      }

    // Log complete timestamp in case of short log timestamp
    val msg = "JS7 Controller terminates now" +
      (termination.restart ?? " and is expected to restart") + s" ($nowString)"
    logger.info(msg)
    printlnWithClock(msg)
    termination
  }

  private def onJavaShutdown(controller: RunningController)(implicit s: Scheduler): Unit = {
    logger.warn("Trying to shut down Controller due to Java shutdown")
    if (!controller.actorSystem.whenTerminated.isCompleted) {
      controller.terminate()
        .runToFuture
        .awaitInfinite
    }
    controller.close()
  }
}

object ControllerMain
{
  // Don't use a Logger here to avoid overwriting a concurrently used logfile
  var _runningSince: Option[Deadline] = None

  def runningSince =
    _runningSince

  def main(args: Array[String]): Unit = {
    val nanoTime = System.nanoTime() // Before anything else, fetch clock
    printlnWithClock(s"JS7 Controller ${BuildInfo.longVersion}")
    _runningSince = Some(Deadline(Duration(nanoTime, NANOSECONDS)))
    StartUp.initializeMain()

    var terminate = ProgramTermination()
    lockAndRunMain(args) { commandLineArguments =>
      terminate = new ControllerMain().run(commandLineArguments)
    }
    if (terminate.restart) {
      System.exit(Js7ReturnCodes.Restart)
    }
  }
}
