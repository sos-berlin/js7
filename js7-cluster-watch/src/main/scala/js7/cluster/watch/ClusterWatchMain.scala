package js7.cluster.watch

import cats.effect.{ExitCode, IO, IOApp}
import js7.base.catsutils.OurIORuntime
import js7.base.utils.ProgramTermination
import js7.common.commandline.CommandLineArguments
import js7.common.system.startup.ServiceMain

object ClusterWatchMain extends IOApp:
  // No Logger here!

  override protected def runtime =
    OurIORuntime.ioRuntime

  def run(args: List[String]) =
    run2(args)(_.untilTerminated)

  def run2(args: List[String])(use: ClusterWatchService => IO[ProgramTermination]): IO[ExitCode] =
    ServiceMain.runAsMain(args, "JS7 ClusterWatch", ClusterWatchConf.fromCommandLine)(
      conf => ClusterWatchService.completeResource(conf),
      use = (_, service: ClusterWatchService) => use(service))
