package js7.master

import js7.base.BuildInfo
import js7.base.time.Timestamp
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Strings._
import js7.common.commandline.CommandLineArguments
import js7.common.configutils.Configs.logConfig
import js7.common.log.ScribeUtils
import js7.common.scalautil.Futures.implicits.SuccessFuture
import js7.common.scalautil.Logger
import js7.core.startup.JavaMain.withShutdownHooks
import js7.core.startup.JavaMainLockfileSupport.lockAndRunMain
import js7.core.startup.StartUp
import js7.core.startup.StartUp.printlnWithClock
import js7.master.configuration.MasterConfiguration
import monix.execution.Scheduler
import scala.concurrent.duration._

/**
  * JS7 Master.
  *
  * @author Joacim Zschimmer
  */
final class MasterMain
{
  private val logger = Logger(getClass)

  def run(arguments: CommandLineArguments): MasterTermination.Terminate = {
    logger.info(s"JS7 Master ${BuildInfo.prettyVersion}")  // Log early for early timestamp and proper logger initialization by a single (not-parallel) call
    logger.debug(arguments.toString)
    val masterConfiguration = MasterConfiguration.fromCommandLine(arguments)
    StartUp.logStartUp(masterConfiguration.configDirectory, Some(masterConfiguration.dataDirectory))
    logConfig(masterConfiguration.config)
    var restartInProcess = false
    var terminate = MasterTermination.Terminate()
    /** val restartJvmWhenDeactivated = masterConfiguration.config.getBoolean("js7.master.cluster.when-deactivated-restart-jvm")
       - Erste HTTP-Anforderungen an deaktivierten Knoten können in ins Leere laufen (mit Timeout abgefangen)
       - Heap platzt nach vielen Deaktivierungen */
    val restartJvmWhenDeactivated = true
    do {
      autoClosing(RunningMaster(masterConfiguration).awaitInfinite) { runningMaster =>
        import runningMaster.scheduler
        withShutdownHooks(masterConfiguration.config, "MasterMain", onJavaShutdown(runningMaster, _)) {
          runningMaster.terminated.awaitInfinite match {
            case t: MasterTermination.Terminate =>
              restartInProcess = false
              terminate = t
            case MasterTermination.Restart =>
              if (restartJvmWhenDeactivated) {
                terminate = MasterTermination.Terminate(restart = true)
              } else {
                logger.info("------- JS7 Master restarts -------")
                restartInProcess = true
              }
          }
        }
      }
    } while (restartInProcess)
    // Log complete timestamp in case of short log timestamp
    val msg = s"JS7 Master terminates at ${Timestamp.now.show}" + (terminate.restart ?: " – will try to restart")
    logger.info(msg)
    printlnWithClock(msg)
    terminate
  }

  private def onJavaShutdown(master: RunningMaster, timeout: FiniteDuration)(implicit s: Scheduler): Unit = {
    logger.warn("Trying to shut down Master due to Java shutdown")
    if (!master.actorSystem.whenTerminated.isCompleted) {
      master.terminate().runToFuture await timeout
    }
    master.close()
  }
}

object MasterMain
{
  // Don't use a Logger here to avoid overwriting a concurrently used logfile

  def main(args: Array[String]): Unit = {
    printlnWithClock(s"JS7 Master ${BuildInfo.prettyVersion}")
    var terminate = MasterTermination.Terminate()
    lockAndRunMain(args) { commandLineArguments =>
      ScribeUtils.coupleScribeWithSlf4j()
      terminate = new MasterMain().run(commandLineArguments)
    }
    if (terminate.restart) {
      System.exit(97)
    }
  }
}
