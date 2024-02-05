package js7.common.system.startup

import cats.effect.{ExitCode, IO, ResourceIO}
import izumi.reflect.Tag
import js7.base.catsutils.OurApp
import js7.base.service.MainService
import js7.base.utils.ProgramTermination
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration

trait ServiceApp extends OurApp:

  protected final def runService[Conf <: BasicConfiguration, S <: MainService : Tag](
    args: List[String],
    name: String,
    argsToConf: CommandLineArguments => Conf,
    useLockFile: Boolean = false)
    (toServiceResource: Conf => ResourceIO[S],
      use: (Conf, S) => IO[ProgramTermination] =
      (_: Conf, service: S) => service.untilTerminated)
  : IO[ExitCode] =
    ServiceMain
      .runAsMain(args, name, argsToConf, useLockFile = useLockFile)(toServiceResource, use)
