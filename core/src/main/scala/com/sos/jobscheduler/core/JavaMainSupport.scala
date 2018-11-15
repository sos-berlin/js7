package com.sos.jobscheduler.core

import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.AutoClosing.multipleAutoClosing
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime.RichDuration
import com.sos.jobscheduler.common.utils.JavaShutdownHook
import com.typesafe.config.Config
import scala.concurrent.duration.Duration

/**
  * @author Joacim Zschimmer
  */
object JavaMainSupport
{
  def runMain(body: ⇒ Unit): Unit =
    try {
      body
      Log4j.shutdown()
    }
    catch { case t: Throwable ⇒
      logger.error(t.toStringWithCauses, t)
      Log4j.shutdown()
      println(s"JOBSCHEDULER TERMINATES DUE TO ERROR: ${t.toStringWithCauses}")
      sys.runtime.exit(1)
    }

  def handleJavaShutdown[A](config: Config, name: String, onJavaShutdown: Duration ⇒ Unit)(body: ⇒ A): A = {
    val hooks = !config.getBoolean("akka.coordinated-shutdown.run-by-jvm-shutdown-hook") list
      JavaShutdownHook.add(name) {
        try onJavaShutdown(config.getDuration("jobscheduler.termination.shutdown-hook-timeout").toFiniteDuration)
        finally Log4j.shutdown()
      }
    multipleAutoClosing(hooks) { _ ⇒
      body
    }
  }

  private val logger = Logger(getClass)
}
