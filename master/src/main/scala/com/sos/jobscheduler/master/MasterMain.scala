package com.sos.jobscheduler.master

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.configutils.Configs.logConfig
import com.sos.jobscheduler.common.log.ScribeUtils
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.startup.JavaMain.withShutdownHooks
import com.sos.jobscheduler.core.startup.JavaMainLockfileSupport.lockAndRunMain
import com.sos.jobscheduler.core.startup.StartUp
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import java.time.LocalDateTime
import monix.execution.Scheduler
import scala.concurrent.duration._

/**
  * JobScheduler Master.
  *
  * @author Joacim Zschimmer
  */
final class MasterMain
{
  private val logger = Logger(getClass)

  def run(arguments: CommandLineArguments): MasterTermination.Terminate = {
    logger.info(s"JobScheduler Master ${BuildInfo.prettyVersion}")  // Log early for early timestamp and proper logger initialization by a single (not-parallel) call
    logger.debug(arguments.toString)
    val masterConfiguration = MasterConfiguration.fromCommandLine(arguments)
    StartUp.logStartUp(masterConfiguration.configDirectory, Some(masterConfiguration.dataDirectory))
    logConfig(masterConfiguration.config)
    var restartInProcess = false
    var terminate = MasterTermination.Terminate()
    /** val restartJvmWhenDeactivated = masterConfiguration.config.getBoolean("jobscheduler.master.cluster.when-deactivated-restart-jvm")
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
                logger.info("------- JobScheduler Master restarts -------")
                restartInProcess = true
              }
          }
        }
      }
    } while (restartInProcess)
    // Log complete timestamp in case of short log timestamp
    val msg = s"JobScheduler Master terminates at ${Timestamp.now.show}"
    logger.info(msg)
    println(s"${LocalDateTime.now.toString.replace('T', ' ')} $msg")
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
    println(s"${LocalDateTime.now.toString.replace('T', ' ')} JobScheduler Master ${BuildInfo.prettyVersion}")
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
