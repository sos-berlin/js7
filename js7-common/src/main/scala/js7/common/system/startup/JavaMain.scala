package js7.common.system.startup

import cats.effect.{Resource, Sync}
import com.typesafe.config.Config
import js7.base.BuildInfo
import js7.base.configutils.Configs.{ConvertibleConfig, logConfig}
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.log.{Log4j, Logger}
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.message.ProblemCodeMessages
import js7.common.system.startup.StartUp.{logJavaSettings, printlnWithClock}
import js7.common.utils.JavaShutdownHook

object JavaMain
{
  private val AkkaShutdownHook = "akka.coordinated-shutdown.run-by-jvm-shutdown-hook"
  private lazy val logger = Logger[this.type]

  def runMain[A](name: String, config: Config)(body: => A): Unit =
    runMain {
      // Log early for early timestamp and proper logger initialization by a single (not-parallel) call
      logger.info(s"$name ${BuildInfo.longVersion}")
      //logger.info(s"config=${conf.configDirectory}")
      logConfig(config)
      logJavaSettings()
      body
    }

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
      if (config.as[Boolean](AkkaShutdownHook, false)) {
        logger.debug(s"JS7 shutdown hook suppressed because Akka has one: $AkkaShutdownHook = on")
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
