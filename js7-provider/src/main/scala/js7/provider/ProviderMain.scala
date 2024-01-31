package js7.provider

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO}
import js7.base.catsutils.{OurApp, OurIORuntime}
import js7.base.utils.ProgramTermination
import js7.common.system.startup.{ServiceApp, ServiceMain}
import js7.controller.ControllerMain.runtime
import js7.controller.RunningController
import js7.controller.configuration.ControllerConfiguration
import js7.provider.configuration.ProviderConfiguration

object ProviderMain extends ServiceApp:
  // No Logger here!

  def run(args: List[String]): IO[ExitCode] =
    given IORuntime = runtime
    runService(
      args,
      "JS7 Controller",
      ProviderConfiguration.fromCommandLine(_),
      useLockFile = true
    )(Provider.resource)
