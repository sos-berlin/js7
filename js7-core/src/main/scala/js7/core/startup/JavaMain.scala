package js7.core.startup

import com.typesafe.config.Config
import js7.base.utils.ScalaUtils.RichThrowable
import js7.common.configutils.Configs.ConvertibleConfig
import js7.common.log.Log4j
import js7.common.scalautil.Logger
import js7.common.time.JavaTimeConverters._
import js7.common.utils.JavaShutdownHook
import js7.core.message.ProblemCodeMessages
import js7.core.startup.StartUp.printlnWithClock
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
    val shutdownHookTimeout = config.getDuration("js7.termination.shutdown-hook-timeout").toFiniteDuration
    if (config.as[Boolean](AkkaShutdownHook, false)) {
      logger.debug(s"JS7 shutdown hook suppressed because Akka has one: $AkkaShutdownHook = on")
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
