package js7.cluster.watch

import cats.effect.{ExitCode, IO}
import js7.base.utils.ProgramTermination
import js7.common.commandline.CommandLineArguments
import js7.common.system.startup.{ServiceApp, ServiceMain}
import org.jetbrains.annotations.TestOnly

object ClusterWatchMain extends ServiceApp:
  // No Logger here!

  def run(args: List[String]) =
    runService(args, "JS7 ClusterWatch", ClusterWatchConf.fromCommandLine)(
      conf => ClusterWatchService.completeResource(conf),
      use = (_, service: ClusterWatchService) => service.untilTerminated)

  @TestOnly
  def runAsTest(args: List[String])
    (use: ClusterWatchService => IO[ProgramTermination])
  : IO[ExitCode] =
    ServiceMain
      .runAsMain(args, "JS7 ClusterWatch",
        ClusterWatchConf.fromCommandLine,
        suppressShutdownLogging = true
      )(conf => ClusterWatchService.completeResource(conf),
        use = (_, service: ClusterWatchService) => use(service))
