package com.sos.jobscheduler.master

import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.JavaMainSupport.{runMain, withShutdownHooks}
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import monix.execution.Scheduler
import scala.concurrent.duration.Duration

/**
  * JobScheduler Master.
  *
  * @author Joacim Zschimmer
  */
object MasterMain {
  private val logger = Logger(getClass)

  def main(args: Array[String]): Unit = {
    logger.info(s"Master ${BuildInfo.prettyVersion}")  // Log early for early timestamp and proper logger initialization by a single (not-parallel) call
    runMain {
      val masterConfiguration = MasterConfiguration.fromCommandLine(args.toVector)
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
  }

  private def onJavaShutdown(master: RunningMaster, timeout: Duration)(implicit s: Scheduler): Unit = {
    logger.warn("Trying to terminate Master due to Java shutdown")
    master.terminate().runToFuture await timeout
    master.close()
  }
}
