package js7.controller

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO, IOApp}
import js7.base.utils.ProgramTermination
import js7.common.system.startup.ServiceMain
import js7.controller.configuration.ControllerConfiguration

object ControllerMain extends IOApp:
  // No Logger here!

  def run(args: List[String]) =
    given IORuntime = runtime
    run2(args)(_.untilTerminated)

  def run2(args: List[String])(use: RunningController => IO[ProgramTermination])
    (using IORuntime)
  : IO[ExitCode] =
    ServiceMain
      .runAsMain(
        args,
        "JS7 Controller",
        ControllerConfiguration.fromCommandLine(_),
        useLockFile = true
      )(
        conf => RunningController.resource(conf),
        use = (_, service: RunningController) => use(service))
