package js7.cluster.watch

import js7.base.utils.ProgramTermination
import js7.common.system.startup.ServiceMain
import monix.eval.Task

object ClusterWatchMain
{
  // No Logger here!

  def main(args: Array[String]): Unit =
    sys.runtime.exit(
      run(args)(_.untilTerminated))

  def run(args: Array[String])(use: ClusterWatchService => Task[ProgramTermination]): Int =
    ServiceMain.intMain(args, "ClusterWatch", ClusterWatchConf.fromCommandLine)(
      ClusterWatchService.resource,
      use = use)
}
