package com.sos.jobscheduler.core

import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime.RichDuration
import com.sos.jobscheduler.common.utils.JavaShutdownHook
import com.sos.jobscheduler.core.message.ProblemCodeMessages
import com.typesafe.config.Config
import scala.collection.immutable.Seq
import scala.concurrent.duration.Duration

/**
  * @author Joacim Zschimmer
  */
object JavaMainSupport
{
  private val logger = Logger(getClass)

  def runMain(body: ⇒ Unit): Unit =
    try {
      ProblemCodeMessages.initialize()
      body
      Log4j.shutdown()
    } catch { case t: Throwable ⇒
      logger.error(t.toStringWithCauses, t)
      Log4j.shutdown()
      println(s"TERMINATING DUE TO ERROR: ${t.toStringWithCauses}")
      sys.runtime.exit(1)
    }

  def withShutdownHooks[A](config: Config, name: String, onJavaShutdown: Duration ⇒ Unit)(body: => A): A = {
    val hooks = addJavaShutdownHooks(config, name, onJavaShutdown)
    try body
    finally hooks foreach (_.close())
  }

  private def addJavaShutdownHooks[A](config: Config, name: String, onJavaShutdown: Duration ⇒ Unit): Seq[JavaShutdownHook] =
    !config.as[Boolean]("akka.coordinated-shutdown.run-by-jvm-shutdown-hook", false) thenList
      JavaShutdownHook.add(name) {
        try onJavaShutdown(config.getDuration("jobscheduler.termination.shutdown-hook-timeout").toFiniteDuration)
        finally Log4j.shutdown()
      }
}
