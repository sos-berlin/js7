package js7.cluster.watch

import cats.effect.{ExitCode, IO}
import js7.base.utils.ProgramTermination
import js7.common.commandline.CommandLineArguments
import js7.common.system.startup.ServiceApp
import org.jetbrains.annotations.TestOnly

object ClusterWatchMain extends ServiceApp:
  // No Logger here!

  override protected val productName = "ClusterWatch"

  def run(args: List[String]): IO[ExitCode] =
    runService(args, ClusterWatchConf.fromCommandLine):
      ClusterWatchService.completeResource

  @TestOnly
  def runAsTest(args: List[String])
    (use: ClusterWatchService => IO[ProgramTermination])
  : IO[ExitCode] =
    runService(args, ClusterWatchConf.fromCommandLine, suppressLogShutdown = true)(
      ClusterWatchService.completeResource,
      use = (_, service: ClusterWatchService) => use(service))
