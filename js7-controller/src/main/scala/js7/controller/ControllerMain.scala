package js7.controller

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO, Resource}
import js7.base.log.Log4j
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
    )(conf =>
      Resource
        .eval(IO:
          /// log4j2.xml: %X{js7.instanceItemId} ///
          Log4j.set("js7.instanceItemId", conf.controllerId.toString))
        .flatMap: _ =>
          RunningController.resource(conf),
      use = (_, service: RunningController) => service.untilTerminated)
