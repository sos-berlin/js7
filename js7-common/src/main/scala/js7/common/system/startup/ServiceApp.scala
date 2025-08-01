package js7.common.system.startup

import cats.effect.{ExitCode, IO, ResourceIO}
import cats.syntax.apply.*
import js7.base.catsutils.OurApp
import js7.base.metering.CallMeterLoggingService
import js7.base.service.{MainService, Service, SimpleMainService}
import js7.base.system.MBeanUtils.registerMBean
import js7.base.utils.AsyncLock.AsyncLockMXBean
import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration

trait ServiceApp extends OurApp:
  self =>

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
        CallMeterLoggingService.resource(cnf.config) *>
          registerMBean("AsyncLock", AsyncLockMXBean) *>
          program(cnf),
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
