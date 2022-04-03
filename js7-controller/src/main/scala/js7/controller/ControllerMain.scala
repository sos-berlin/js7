package js7.controller

import js7.base.BuildInfo
import js7.base.configutils.Configs.logConfig
import js7.base.log.Logger
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.common.commandline.CommandLineArguments
import js7.common.system.startup.JavaMain.withShutdownHooks
import js7.common.system.startup.JavaMainLockfileSupport.lockAndRunMain
import js7.common.system.startup.StartUp
import js7.common.system.startup.StartUp.{logJavaSettings, printlnWithClock, startUpLine}
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
    logger.info("JS7 Controller " + BuildInfo.longVersion +  // Log early for early timestamp and proper logger initialization by a single (not-parallel) call
      "\n" + "━" * 80)  // In case, the previous file is appended
    logger.info(startUpLine())
    logger.debug(arguments.toString)
    val conf = ControllerConfiguration.fromCommandLine(arguments)
    logger.info(s"${conf.controllerId} config=${conf.configDirectory} data=${conf.dataDirectory}")
    logConfig(conf.config)
    logJavaSettings()
    var restartInProcess = false
    var terminate = ProgramTermination()
    /** val restartJvmWhenDeactivated = conf.config.getBoolean("js7.journal.cluster.when-deactivated-restart-jvm")
       - Erste HTTP-Anforderungen an deaktivierten Knoten können in ins Leere laufen (mit Timeout abgefangen)
       - Heap platzt nach vielen Deaktivierungen */
    val restartJvmWhenDeactivated = true
    do {
      autoClosing(RunningController(conf).awaitInfinite) { runningController =>
        import runningController.scheduler
        withShutdownHooks(conf.config, "ControllerMain", () => onJavaShutdown(runningController)) {
          runningController.terminated.awaitInfinite match {
            case t: ProgramTermination =>
              if (!t.restart) {
                terminate = t
              } else {
                if (restartJvmWhenDeactivated) {
                  terminate = t
                } else {
                  // Does not work
                  logger.info("------- JS7 Controller restarts -------")
                  restartInProcess = true
                }
              }
          }
        }
      }
    } while (restartInProcess)
    // Log complete timestamp in case of short log timestamp
    val msg = s"JS7 Controller terminates now" + (terminate.restart ?? " and is expected to restart")
    logger.info(msg)
    printlnWithClock(msg)
    terminate
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
      System.exit(97)
    }
  }
}
