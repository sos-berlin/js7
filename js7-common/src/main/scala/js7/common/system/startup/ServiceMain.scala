package js7.common.system.startup

import cats.effect.{Resource, Sync, SyncIO}
import cats.syntax.flatMap.*
import com.typesafe.config.Config
import js7.base.BuildInfo
import js7.base.configutils.Configs.{ConvertibleConfig, logConfig}
import js7.base.log.Logger.syntax.*
import js7.base.log.{Log4j, Logger}
import js7.base.service.{MainService, Service}
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, ProgramTermination}
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration
import js7.common.system.ThreadPools
import js7.common.system.startup.Js7ReturnCodes.terminationToExitCode
import js7.common.system.startup.StartUp.{logJavaSettings, nowString, printlnWithClock, startUpLine}
import js7.common.utils.JavaShutdownHook
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.duration.{Deadline, Duration, NANOSECONDS}

object ServiceMain
{
  // Don't use a Logger here to avoid overwriting a concurrently used logfile !!!
  private val AkkaShutdownHook = "akka.coordinated-shutdown.run-by-jvm-shutdown-hook"

  var _runningSince: Option[Deadline] = None

  def runningSince: Option[Deadline] =
    _runningSince

  def readyMessageWithLine(prefix: String): String =
    prefix +
      ServiceMain.runningSince.fold("")(o => s" (after ${o.elapsed.pretty})") +
      "\n" + "─" * 80

  def main[Conf <: BasicConfiguration, S <: MainService](
    args: Array[String],
    name: String,
    argsToConf: CommandLineArguments => Conf,
    useLockFile: Boolean = false)
    (toResource: (Conf, Scheduler) => Resource[Task, S],
      use: S => Task[ProgramTermination] = (_: S).untilTerminated)
  : Unit = {
    val exitCode = intMain(args, name, argsToConf, useLockFile = useLockFile)(toResource, use)
    if (exitCode != 0) {
      sys.runtime.exit(exitCode)
    }
  }

  def intMain[Conf <: BasicConfiguration, S <: MainService](
    args: Array[String],
    name: String,
    argsToConf: CommandLineArguments => Conf,
    useLockFile: Boolean = false)
    (toServiceResource: (Conf, Scheduler) => Resource[Task, S],
      use: S => Task[ProgramTermination] = (_: S).untilTerminated)
  : Int = {
    startUp(name)
    handleProgramTermination(name) {
      def body(commandLineArguments: CommandLineArguments) = {
        lazy val conf = {
          val conf = argsToConf(commandLineArguments)
          commandLineArguments.requireNoMoreArguments() // throws
          conf
        }
        withLogger.logFirstLines(name, commandLineArguments, conf)
        withLogger.blockingRun(name, conf.config)(
          service = toServiceResource(conf, _),
          use = use)
      }

      JavaMainLockfileSupport.runMain(args, useLockFile = useLockFile)(body)
    }
  }

  def startUp(name: String): Unit = {
    // Do not use Logger here !!!  Logger will be initialized later
    val nanoTime = System.nanoTime() // Before anything else, fetch clock
    printlnWithClock(s"JS7 $name ${BuildInfo.longVersion}")
    _runningSince = Some(Deadline(Duration(nanoTime, NANOSECONDS)))
    StartUp.initializeMain()
  }

  def handleProgramTermination(name: String)(body: => ProgramTermination): Int =
    try {
      val termination = body
      withLogger.onProgramTermination(name, termination)
    } catch withLogger.catcher

  /** For usage after logging system has properly been initialized. */
  object withLogger {
    private lazy val logger = Logger[ServiceMain.type]

    def onProgramTermination(name: String, termination: ProgramTermination): Int =
      try {
        // Log complete timestamp in case of short log timestamp
        val msg = s"JS7 $name terminates now" +
          (termination.restart ?? " and is expected to restart") + s" ($nowString)"
        logger.info(msg)
        printlnWithClock(msg)

        terminationToExitCode(termination)
      } catch catcher

    def catcher: PartialFunction[Throwable, Int] = {
      case t: Throwable =>
        logger.error(t.toStringWithCauses, t.nullIfNoStackTrace)
        System.err.println(t.toStringWithCauses)
        t.printStackTrace(System.err)
        1
    }

    def logFirstLines(
      name: String,
      commandLineArguments: CommandLineArguments,
      conf: => BasicConfiguration)
    : Unit = {
      // Log early for early timestamp and proper logger initialization by a
      // single (non-concurrent) call
      // Log a bar, in case the previous file is appended
      logger.info("JS7 " + name + " " + BuildInfo.longVersion +
        "\n" + "━" * 80)
      logger.info(startUpLine())
      logger.debug(commandLineArguments.toString)

      val paths = conf.maybeConfigDirectory.map(o => s"config=$o") ++
        conf.maybeDataDirectory.map(o => s"data=$o")
      if (paths.nonEmpty) logger.info(paths.mkString(" "))

      logConfig(conf.config)
      logJavaSettings()
    }

    def blockingRun[S <: MainService](
      name: String,
      config: Config,
      timeout: Duration = Duration.Inf)(
      service: Scheduler => Resource[Task, S],
      use: S => Task[ProgramTermination] = (_: S).untilTerminated)
    : ProgramTermination =
      ThreadPools.standardSchedulerResource[SyncIO](name, config)
        .use(implicit scheduler => SyncIO(
          resource(name, config)(service(scheduler))
            .use(use)
            .await(timeout)))
        .unsafeRunSync()

    private def resource[S <: MainService](name: String, config: Config)
      (serviceResource: Resource[Task, S])
      (implicit scheduler: Scheduler)
    : Resource[Task, S] =
      serviceResource
        .toAllocatedResource
        .flatTap(allocated =>
          shutdownHookResource[Task](config, name)(onJavaShutdown(allocated)))
        .map(_.allocatedThing)

    private def onJavaShutdown[S <: Service](allocatedService: Allocated[Task, S])
      (implicit s: Scheduler)
    : Unit = {
      logger.warn(s"Trying to shut down JS7 $allocatedService due to Java shutdown")
      val stop = logger.debugTask("onJavaShutDown stop service")(
        allocatedService.stop)
      for (t <- stop.attempt.runSyncUnsafe().left) {
        logger.error(s"onJavaShutdown: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
      }
    }

    def shutdownHookResource[F[_] : Sync](config: Config, name: String)(onJavaShutdown: => Unit)
    : Resource[F, Unit] =
      Resource
        .fromAutoCloseable(Sync[F].delay(
          addJavaShutdownHook(config, name, () => onJavaShutdown)))
        .map(_ => ())

    private def addJavaShutdownHook(config: Config, name: String, onJavaShutdown: () => Unit)
    : AutoCloseable = {
      val maybeHook =
        if (config.as[Boolean](AkkaShutdownHook, false)) {
          logger.debug(s"JS7 shutdown hook suppressed because Akka has one: $AkkaShutdownHook = on")
          None
        } else
          Some(JavaShutdownHook.add(name) {
            try onJavaShutdown()
            catch {
              case t: Throwable =>
                logger.debug(t.toStringWithCauses, t)
                throw t
            }
            finally Log4j.shutdown()
          })

      new AutoCloseable {
        def close() = maybeHook.foreach(_.close())
      }
    }
  }
}
