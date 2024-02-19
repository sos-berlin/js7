package js7.cluster.watch

import cats.effect.{ExitCode, IO}
import js7.base.catsutils.OurApp
import js7.base.utils.ProgramTermination
import js7.common.commandline.CommandLineArguments
import js7.common.system.startup.ServiceMain

object ClusterWatchMain extends OurApp:
  // No Logger here!

  def run(args: List[String]) =
    run2(args)(_.untilTerminated)

  def run2(args: List[String], suppressShutdownLogging: Boolean = false)
    (use: ClusterWatchService => IO[ProgramTermination])
  : IO[ExitCode] =
    ServiceMain
      .runAsMain(
        args, "JS7 ClusterWatch",
        ClusterWatchConf.fromCommandLine,
        suppressShutdownLogging = suppressShutdownLogging
      )(conf => ClusterWatchService.completeResource(conf),
        use = (_, service: ClusterWatchService) => use(service))
