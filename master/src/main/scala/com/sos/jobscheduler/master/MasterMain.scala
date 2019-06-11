package com.sos.jobscheduler.master

import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.startup.JavaMainLockfileSupport.lockAndRunMain
import com.sos.jobscheduler.core.startup.JavaMainSupport.withShutdownHooks
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import java.time.LocalTime
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

  def run(arguments: CommandLineArguments): Unit = {
    logger.info(s"JobScheduler Master ${BuildInfo.prettyVersion}")  // Log early for early timestamp and proper logger initialization by a single (not-parallel) call
    val masterConfiguration = MasterConfiguration.fromCommandLine(arguments)
    autoClosing(RunningMaster(masterConfiguration).awaitInfinite) { master =>
      import master.scheduler
      withShutdownHooks(masterConfiguration.config, "MasterMain", onJavaShutdown(master, _)) {
        master.terminated.awaitInfinite
      }
    }
    val msg = "JobScheduler Master terminates"
    logger.info(msg)
    println(msg)
  }

  private def onJavaShutdown(master: RunningMaster, timeout: FiniteDuration)(implicit s: Scheduler): Unit = {
    logger.warn("Trying to terminate Master due to Java shutdown")
    master.terminate().runToFuture await timeout
    master.close()
  }
}

object MasterMain
{
  // Don't use a Logger here to avoid overwriting a concurrently used logfile

  def main(args: Array[String]): Unit = {
    println(s"${LocalTime.now.toString take 12} JobScheduler Master ${BuildInfo.prettyVersion}")
    lockAndRunMain(args) { commandLineArguments =>
      new MasterMain().run(commandLineArguments)
    }
  }
}
