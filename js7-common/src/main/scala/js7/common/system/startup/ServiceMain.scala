package js7.common.system.startup

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO, ResourceIO}
import izumi.reflect.Tag
import js7.base.configutils.Configs.logConfig
import js7.base.log.{Log4j, Logger}
import js7.base.service.{MainService, MainServiceTerminationException}
import js7.base.system.startup.StartUp
import js7.base.system.startup.StartUp.{logJavaSettings, nowString, printlnWithClock}
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.utils.AllocatedForJvm.useSync
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.{BuildInfo, utils}
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration
import scala.concurrent.duration.{Deadline, Duration}
import scala.util.control.NonFatal

object ServiceMain:
  private lazy val logger = Logger[this.type]
  private val runningSince: Deadline = Deadline.now

  // Run initialization code above.
  def initialize(): Unit = ()

  /** Returns the return code. */
  def runAsMain[Cnf <: BasicConfiguration, Svc <: MainService: Tag](
    args: Seq[String],
    name: String,
    argsToConf: CommandLineArguments => Cnf,
    useLockFile: Boolean = false,
    suppressTerminationLogging: Boolean = false,
    suppressLogShutdown: Boolean = false)
    (toServiceResource: Cnf => ResourceIO[Svc],
    use: (Cnf, Svc) => IO[ProgramTermination] = (_: Cnf, service: Svc) => service.untilTerminated)
  : IO[ExitCode] =
    IO
      .defer:
        printlnWithClock(s"JS7 $name ${BuildInfo.longVersion}")
        StartUp.initializeMain()

        JavaMainLockfileSupport
          .runMain(args.toVector, useLockFile = useLockFile): commandLineArguments =>
            IO.defer:
              lazy val conf: Cnf =
                try
                  val conf = argsToConf(commandLineArguments)
                  commandLineArguments.requireNoMoreArguments() // throws
                  conf
                catch case NonFatal(t) =>
                  // We have to log the error ourselves, like a Service
                  logger.error(t.toStringWithCauses)
                  throw t
              logging.logFirstLines(commandLineArguments, conf)
              logging.run(toServiceResource(conf)):
                use(conf, _)
          .attempt.map:
            case Left(throwable) =>
              logger.debug(s"Already logged: ❓${throwable.toStringWithCauses}")
              ExitCode.Error // Service has already logged an error
            case Right(termination) =>
              if !suppressTerminationLogging then
                logging.logTermination(name, termination)
              termination.toExitCode
      .guarantee:
        IO.unlessA(suppressLogShutdown)(IO:
          Log4j.shutdown(suppressLogging = suppressTerminationLogging))

  def blockingRun[Svc <: MainService](
    timeout: Duration = Duration.Inf)
    (using rt: IORuntime, S: Tag[Svc])
    (resource: ResourceIO[Svc],
    use: Svc => ProgramTermination = (_: Svc)
      .untilTerminated
      .map(o => o: ProgramTermination) // because we have no Tag[Svc#Termination]
      .await(timeout))
  : ProgramTermination =
    resource
      .toAllocated
      .await(timeout)
      .useSync(timeout + 1.s)(use)

  def readyMessageWithLine(prefix: String): String =
    prefix +
      s" (after ${runningSince.elapsed.pretty})" +
      "\n" + "─" * 80

  /** For usage after logging system has properly been initialized. */
  private object logging:
    private lazy val logger =
      Logger.initialize("JS7 Engine") // Just in case it has not yet been initialized
      Logger[ServiceMain.type]

    def logTermination(name: String, termination: ProgramTermination): ExitCode =
      // Log complete timestamp in case of short log timestamp
      val msg = s"$name terminates after ${runningSince.elapsed.pretty}" +
        (termination.restart ?? " and is expected to restart") + s" ($nowString)"
      logger.info(msg)
      printlnWithClock(msg)
      termination.toExitCode

    def logFirstLines(commandLineArguments: CommandLineArguments, conf: => BasicConfiguration)
    : Unit =
      logger.debug(commandLineArguments.toString)

      val paths = conf.maybeConfigDirectory.map(o => s"config=$o") ++
        conf.maybeDataDirectory.map(o => s"data=$o")
      if paths.nonEmpty then logger.info(paths.mkString(" "))

      logConfig(conf.config)
      logJavaSettings()

    private[ServiceMain] def run[Svc <: MainService: Tag](
      resource: ResourceIO[Svc])
      (use: Svc => IO[ProgramTermination])
    : IO[ProgramTermination] =
      resource
        .use: service =>
          use(service)
        .recover:
          case t: MainServiceTerminationException =>
            logger.debug(t.toStringWithCauses)
            logger.info(t.getMessage)
            t.termination
