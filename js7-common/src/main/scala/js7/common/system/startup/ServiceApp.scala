package js7.common.system.startup

import cats.effect.{ExitCode, IO, ResourceIO}
import izumi.reflect.Tag
import js7.base.catsutils.OurApp
import js7.base.service.{MainService, SimpleMainService}
import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration

trait ServiceApp extends OurApp:

  protected final def runProgramAsService[Cnf <: BasicConfiguration](
    args: List[String],
    toConf: CommandLineArguments => Cnf,
    label: String = getClass.shortClassName)
    (program: Cnf => IO[ExitCode])
  : IO[ExitCode] =
    runService[Cnf, MainService](args, toConf):
      conf => SimpleMainService.resource(
        program = program(conf).map(ProgramTermination.fromExitCode),
        label = label)

  protected final def runService[Cnf <: BasicConfiguration, Svc <: MainService : Tag](
    args: List[String],
    argsToConf: CommandLineArguments => Cnf,
    useLockFile: Boolean = false,
    suppressShutdownLogging: Boolean = false)
    (program: Cnf => ResourceIO[Svc],
      use: (Cnf, Svc) => IO[ProgramTermination] = (_: Cnf, service: Svc) => service.untilTerminated)
  : IO[ExitCode] =
    ServiceMain.runAsMain(
        args, productName,
        argsToConf,
        useLockFile = useLockFile,
        suppressShutdownLogging = suppressShutdownLogging)(
      program, use)
