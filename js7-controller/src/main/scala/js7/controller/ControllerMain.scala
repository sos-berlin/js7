package js7.controller

import cats.effect.{ExitCode, IO}
import cats.effect.unsafe.IORuntime
import js7.common.system.startup.ServiceApp
import js7.controller.configuration.ControllerConfiguration

object ControllerMain extends ServiceApp:
  // No Logger here!

  def run(args: List[String]): IO[ExitCode] =
    given IORuntime = runtime
    runService(
      args,
      "JS7 Controller",
      ControllerConfiguration.fromCommandLine(_),
      useLockFile = true
    )(conf => RunningController.resource(conf),
      use = (_, service: RunningController) => service.untilTerminated)
