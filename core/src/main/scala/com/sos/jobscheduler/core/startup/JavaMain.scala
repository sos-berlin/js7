package com.sos.jobscheduler.core.startup

import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.JavaTimeConverters._
import com.sos.jobscheduler.common.utils.JavaShutdownHook
import com.sos.jobscheduler.core.message.ProblemCodeMessages
import com.sos.jobscheduler.core.startup.StartUp.printlnWithClock
import com.typesafe.config.Config
import scala.concurrent.duration.FiniteDuration

object JavaMain
{
  private val AkkaShutdownHook = "akka.coordinated-shutdown.run-by-jvm-shutdown-hook"
  private val logger = Logger(getClass)

  def runMain(body: => Unit): Unit =
    try {
      ProblemCodeMessages.initialize()
      body
      Log4j.shutdown()
    } catch { case t: Throwable =>
      logger.error(t.toStringWithCauses, t)
      Log4j.shutdown()
      printlnWithClock(s"TERMINATING DUE TO ERROR: ${t.toStringWithCauses}")
      sys.runtime.exit(1)
    }

  def withShutdownHooks[A](config: Config, name: String, onJavaShutdown: FiniteDuration => Unit)(body: => A): A = {
    val hooks = addJavaShutdownHooks(config, name, onJavaShutdown)
    try body
    finally hooks foreach (_.close())
  }

  private def addJavaShutdownHooks[A](config: Config, name: String, onJavaShutdown: FiniteDuration => Unit): Seq[JavaShutdownHook] = {
    val shutdownHookTimeout = config.getDuration("jobscheduler.termination.shutdown-hook-timeout").toFiniteDuration
    if (config.as[Boolean](AkkaShutdownHook, false)) {
      logger.debug(s"JobScheduler shutdown hook suppressed because Akka has one: $AkkaShutdownHook = on")
      Nil
    } else
      JavaShutdownHook.add(name) {
        try onJavaShutdown(shutdownHookTimeout)
        catch { case t: Throwable =>
          logger.debug(t.toStringWithCauses, t)
          throw t
        }
        finally Log4j.shutdown()
      } :: Nil
  }
}
