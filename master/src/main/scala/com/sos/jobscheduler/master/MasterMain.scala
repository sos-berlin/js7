package com.sos.jobscheduler.master

import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.JavaShutdownHook
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand
import monix.execution.Scheduler.Implicits.global
import scala.util.control.NonFatal

/**
  * JobScheduler Master.
  *
  * @author Joacim Zschimmer
  */
object MasterMain {
  private val OrderScheduleDuration = 1 * 60.s
  private val ShutdownTimeout = 10.s
  private val logger = Logger(getClass)

  def main(args: Array[String]): Unit = {
    logger.info(s"Master ${BuildInfo.buildVersion}")  // Log early
    try {
      val masterConfiguration = MasterConfiguration.fromCommandLine(args.toVector)
      autoClosing(RunningMaster(masterConfiguration).awaitInfinite) { master ⇒
        try master.executeCommandAsSystemUser(MasterCommand.ScheduleOrdersEvery(OrderScheduleDuration.toFiniteDuration)).runAsync // Will block on recovery until Agents are started: await 99.s
        catch { case NonFatal(t) ⇒ logger.error(s"ScheduleOrdersEvery FAILED: ${t.toStringWithCauses}") }  // SchedulerOrdersEvery ist nur zum Test
        autoClosing(JavaShutdownHook.add("MasterMain")(onJavaShutdown(master))) { _ ⇒  // TODO Interfers with Akkas CoordinatedShutdown shutdown hook ?
          master.terminated.awaitInfinite
        }
      }
      val msg = "JobScheduler Master terminates"
      logger.info(msg)
      Log4j.shutdown()
      println(msg)
    }
    catch { case t: Throwable ⇒
      logger.error(t.toString, t)
      Log4j.shutdown()
      println(s"JOBSCHEDULER MASTER TERMINATES DUE TO ERROR: ${t.toStringWithCauses}")
      sys.runtime.exit(1)
    }
  }

  private def onJavaShutdown(master: RunningMaster): Unit =
    try {
      logger.warn("Trying to terminate Master due to Java shutdown")
      master.terminate().runAsync await ShutdownTimeout
      master.close()
    } finally
      Log4j.shutdown()
}
