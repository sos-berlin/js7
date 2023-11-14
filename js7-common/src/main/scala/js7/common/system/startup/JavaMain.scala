package js7.common.system.startup

import cats.effect.{Resource, Sync}
import com.typesafe.config.Config
import js7.base.configutils.Configs.{ConvertibleConfig, logConfig}
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.log.{Log4j, Logger}
import js7.base.system.startup.StartUp.{logJavaSettings, printlnWithClock, startUpLine}
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.commandline.CommandLineArguments
import js7.common.message.ProblemCodeMessages
import js7.common.utils.JavaShutdownHook

object JavaMain
{
  private val PekkoShutdownHook = "pekko.coordinated-shutdown.run-by-jvm-shutdown-hook"
  private lazy val logger = Logger[this.type]

  def runMain[A](name: String, arguments: => CommandLineArguments, config: => Config)(body: => A)
  : Unit =
    runMain(name) {
      logger.debug(arguments.toString)
      logConfig(config)
      logJavaSettings()
      body
    }

  def runMain(name: String)(body: => Unit): Unit =
    try {
      Log4j.initialize(name)
      // Log early for early timestamp and proper logger initialization by a
      // single (non-concurrent) call
      logger.info(startUpLine(name))
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

  def withShutdownHooks[A](config: Config, name: String, onJavaShutdown: () => Unit)(body: => A): A =
    autoClosing(addJavaShutdownHooks(config, name, onJavaShutdown))(_ =>
      body)

  def shutdownHookResource[F[_] : Sync](config: Config, name: String)(onJavaShutdown: => Unit)
  : Resource[F, Unit] =
    Resource
      .fromAutoCloseable(Sync[F].delay(
        addJavaShutdownHooks(config, name, () => onJavaShutdown)))
      .map(_ => ())

  private def addJavaShutdownHooks(config: Config, name: String, onJavaShutdown: () => Unit): AutoCloseable = {
    val hooks =
      if (config.as[Boolean](PekkoShutdownHook, false)) {
        logger.debug(s"JS7 shutdown hook suppressed because Pekko has one: $PekkoShutdownHook = on")
        Nil
      } else
        JavaShutdownHook.add(name) {
          try onJavaShutdown()
          catch {
            case t: Throwable =>
              logger.debug(t.toStringWithCauses, t)
              throw t
          }
          finally Log4j.shutdown()
        } :: Nil

    new AutoCloseable {
      def close() = hooks.foreach(_.close())
    }
  }
}
