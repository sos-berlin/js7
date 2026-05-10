package js7.common.system.startup

import cats.effect.unsafe.IORuntimeConfig
import cats.effect.{ExitCode, IO, ResourceIO}
import js7.base.catsutils.OurApp
import js7.base.service.{MainService, Service, SimpleMainService}
import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.{RichAny, RichJavaClass}
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration
import js7.common.system.startup.ServiceApp.*
import scala.concurrent.duration.Duration

trait ServiceApp extends OurApp:
  self =>

  override def runtimeConfig: IORuntimeConfig =
    super.runtimeConfig
      .pipeIf(useOwnResponsivenessMeter):
        _.copy(
          cpuStarvationCheckInitialDelay = Duration.Inf)

  protected final def runProgramAsService[Cnf <: BasicConfiguration](
    args: List[String],
    toConf: CommandLineArguments => Cnf)
    (program: Cnf => IO[ExitCode | Unit])
  : IO[ExitCode] =
    runService[Cnf, MainService](args, toConf, suppressTerminationLogging = true):
      conf => programAsService(program(conf))

  protected final def runService[Cnf <: BasicConfiguration, Svc <: MainService](
    args: List[String],
    argsToConf: CommandLineArguments => Cnf,
    useLockFile: Boolean = false,
    suppressTerminationLogging: Boolean = false,
    suppressLogShutdown: Boolean = false)
    (program: Cnf => ResourceIO[Svc],
      use: (Cnf, Svc) => IO[ProgramTermination] = (_: Cnf, service: Svc) => service.untilTerminated)
  : IO[ExitCode] =
    ServiceMain.runAsMain(
        args, productName, argsToConf,
        useLockFile = useLockFile,
        suppressTerminationLogging = suppressTerminationLogging,
        suppressLogShutdown = suppressLogShutdown)(
      toServiceResource = cnf =>
        for
          _ <- MainSupportService.service(cnf.config, useOwnResponsivenessMeter = true)
          svc <- program(cnf)
        yield
          svc,
      use = use)

  protected final def runSimpleService[Cnf <: BasicConfiguration, Svc <: MainService](
    args: List[String],
    argsToConf: CommandLineArguments => Cnf,
    useLockFile: Boolean = false,
    suppressTerminationLogging: Boolean = false,
    suppressLogShutdown: Boolean = false)
    (program: Cnf => IO[Unit | ExitCode | ProgramTermination])
  : IO[ExitCode] =
    runService(args, argsToConf,
      useLockFile = useLockFile,
      suppressTerminationLogging = suppressTerminationLogging,
      suppressLogShutdown = suppressLogShutdown
    ): conf =>
      Service:
        Service.simple(program(conf))

  protected final def programAsService(program: IO[ExitCode | Unit])
  : ResourceIO[SimpleMainService] =
    SimpleMainService.service(program, label = self.toString)

  override def toString = getClass.shortClassName


object ServiceApp:
  private val useOwnResponsivenessMeter = true
