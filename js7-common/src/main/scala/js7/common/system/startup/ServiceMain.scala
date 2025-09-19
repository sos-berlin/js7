package js7.common.system.startup

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO, ResourceIO}
import izumi.reflect.Tag
import js7.base.BuildInfo
import js7.base.configutils.Configs.logConfig
import js7.base.log.Logger
import js7.base.service.{MainService, MainServiceTerminationException}
import js7.base.system.startup.StartUp.{logJavaSettings, printlnWithClock}
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.utils.AllocatedForJvm.useSync
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isTest
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration
import scala.concurrent.duration.{Deadline, Duration}
import scala.util.control.NonFatal

object ServiceMain:
  private lazy val logger = Logger[this.type]
  private val runningSince: Deadline = Deadline.now

  /** Returns the return code. */
  def runAsMain[Cnf <: BasicConfiguration, Svc <: MainService](
    args: Seq[String],
    productName: String,
    argsToConf: CommandLineArguments => Cnf,
    useLockFile: Boolean = false,
    suppressTerminationLogging: Boolean = false,
    suppressLogShutdown: Boolean = false)
    (toServiceResource: Cnf => ResourceIO[Svc],
    use: (Cnf, Svc) => IO[ProgramTermination] = (_: Cnf, service: Svc) => service.untilTerminated)
  : IO[ExitCode] =
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
            logging.logTermination(productName, termination)
          termination.toExitCode
      .guarantee:
        IO:
          if !suppressLogShutdown then
            Logger.shutdown(suppressLogging = suppressTerminationLogging)
      .flatTap: exitCode =>
        if exitCode.code != 0 && catsEffectIgnoresExitCode && !isTest/*dont exit tests!*/ then
          //blocks? IO(System.exit(exitCode.code))
          IO(sys.runtime.halt(exitCode.code))
        else
          IO.unit

  private def catsEffectIgnoresExitCode: Boolean =
    BuildInfo.catsEffectVersion.startsWith("3.5.") && Runtime.version.feature > 17

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
      if Logger.initialize("JS7 Engine") then
        Logger.trace(s"❌ Logger has been initialized implicitly")
      Logger[ServiceMain.type]

    def logTermination(name: String, termination: ProgramTermination): Unit =
      // Log complete timestamp in case of short log timestamp
      val msg = s"$name terminates ${runningSince.elapsed.pretty} after start" +
        (termination.restart ?? " and is expected to restart")
      logger.info(msg)
      printlnWithClock(msg)

    def logFirstLines(commandLineArguments: CommandLineArguments, conf: => BasicConfiguration)
    : Unit =
      logger.debug(commandLineArguments.toString)

      val paths = conf.maybeConfigDirectory.map(o => s"config=$o") ++
        conf.maybeDataDirectory.map(o => s"data=$o")
      if paths.nonEmpty then logger.info(paths.mkString(" "))

      logConfig(conf.config)
      logJavaSettings()

    private[ServiceMain] def run[Svc <: MainService](
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
