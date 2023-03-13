package js7.controller

import js7.common.system.startup.ServiceMain
import js7.controller.configuration.ControllerConfiguration

object ControllerMain
{
  // No Logger here!

  def main(args: Array[String]): Unit =
    ServiceMain.mainMainThenExit[ControllerConfiguration, RunningController](
      args,
      "Controller",
      ControllerConfiguration.fromCommandLine(_),
      useLockFile = true)(
      RunningController.resource(_)(_))
}
