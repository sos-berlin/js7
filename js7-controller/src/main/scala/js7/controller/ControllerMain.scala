package js7.controller

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO}
import js7.common.system.startup.ServiceApp
import js7.controller.configuration.ControllerConfiguration

object ControllerMain extends ServiceApp:

  // No Logger here!
  override protected def productName = "JS7 Controller"

  def run(args: List[String]): IO[ExitCode] =
    given IORuntime = runtime
    runService(args, ControllerConfiguration.fromCommandLine(_), useLockFile = true):
      RunningController.resource(_)
