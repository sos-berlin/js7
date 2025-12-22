package js7.common.system.startup

import cats.effect.unsafe.IORuntimeConfig
import cats.effect.{ExitCode, IO, ResourceIO}
import cats.implicits.catsSyntaxApplicativeByName
import js7.base.catsutils.OurApp
import js7.base.metering.{CallMeterLoggingService, ResponsivenessMeter}
import js7.base.service.{MainService, Service, SimpleMainService}
import js7.base.system.MBeanUtils.registerStaticMBean
import js7.base.system.ThreadsMXBean
import js7.base.utils.ScalaUtils.syntax.{RichAny, RichJavaClass}
import js7.base.utils.{AsyncLock, ProgramTermination}
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration
import js7.common.http.HttpMXBean
import js7.common.pekkohttp.web.session.SessionRegister
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
        args, productName,
        argsToConf,
        useLockFile = useLockFile,
        suppressTerminationLogging = suppressTerminationLogging,
        suppressLogShutdown = suppressLogShutdown)(
      cnf =>
        for
          _ <- CallMeterLoggingService.service(cnf.config)
          _ <- ResponsivenessMeter.service(cnf.config).whenA(useOwnResponsivenessMeter)
          _ <- registerStaticMBean("Threads", ThreadsMXBean.Bean)
          _ <- registerStaticMBean("AsyncLock", AsyncLock.Bean)
          _ <- registerStaticMBean("HttpMXBean", HttpMXBean.Bean)
          _ <- registerStaticMBean("Sessions", SessionRegister.Bean)
          svc <- program(cnf)
        yield svc,
      use)

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
      Service.resource:
        Service.simple(program(conf))

  protected final def programAsService(program: IO[ExitCode | Unit])
  : ResourceIO[SimpleMainService] =
    SimpleMainService.resource(program, label = self.toString)

  override def toString = getClass.shortClassName


object ServiceApp:
  private val useOwnResponsivenessMeter = true
