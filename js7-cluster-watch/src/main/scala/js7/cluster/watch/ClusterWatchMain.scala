package js7.cluster.watch

import js7.base.io.process.ReturnCode
import js7.base.utils.ProgramTermination
import js7.common.system.startup.{JavaMain, ServiceMain}
import monix.eval.Task

object ClusterWatchMain
{
  // No Logger here!

  def main(args: Array[String]): Unit = {
    val returnCode = run(args)(_.untilTerminated)
    JavaMain.exitIfNonZero(returnCode)
  }

  def run(args: Array[String])(use: ClusterWatchService => Task[ProgramTermination]): ReturnCode =
    ServiceMain.returnCodeMain(args, "ClusterWatch", ClusterWatchConf.fromCommandLine)(
      (conf, _) => ClusterWatchService.completeResource(conf),
      use = use)
}
