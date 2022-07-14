package js7.common.system.startup

import com.typesafe.config.Config
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.log.{Log4j, Logger}
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.message.ProblemCodeMessages
import js7.common.system.startup.StartUp.printlnWithClock
import js7.common.utils.JavaShutdownHook

object JavaMain
{
  private val AkkaShutdownHook = "akka.coordinated-shutdown.run-by-jvm-shutdown-hook"
  private lazy val logger = Logger[this.type]

  def runMain(body: => Unit): Unit =
    try {
      Log4j.initialize()
      coupleScribeWithSlf4j()
      ProblemCodeMessages.initialize()
      // Initialize class and object for possible quicker emergency stop
      Halt.initialize()
      body
      Log4j.shutdown()
    } catch { case t: Throwable =>
      logger.error(t.toStringWithCauses, t)
      Log4j.shutdown()
      printlnWithClock(s"TERMINATING DUE TO ERROR: ${t.toStringWithCauses}")
      sys.runtime.exit(1)
    }

  def withShutdownHooks[A](config: Config, name: String, onJavaShutdown: () => Unit)(body: => A): A = {
    val hooks = addJavaShutdownHooks(config, name, onJavaShutdown)
    try body
    finally hooks foreach (_.close())
  }

  private def addJavaShutdownHooks[A](config: Config, name: String, onJavaShutdown: () => Unit): Seq[JavaShutdownHook] = {
    if (config.as[Boolean](AkkaShutdownHook, false)) {
      logger.debug(s"JS7 shutdown hook suppressed because Akka has one: $AkkaShutdownHook = on")
      Nil
    } else
      JavaShutdownHook.add(name) {
        try onJavaShutdown()
        catch { case t: Throwable =>
          logger.debug(t.toStringWithCauses, t)
          throw t
        }
        finally Log4j.shutdown()
      } :: Nil
  }
}
