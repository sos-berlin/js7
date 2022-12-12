package js7.cluster.watch

import cats.effect.Resource
import js7.base.BuildInfo
import js7.cluster.watch.api.HttpClusterNodeApi
import js7.common.akkautils.Akkas.actorSystemResource
import js7.common.commandline.CommandLineArguments
import js7.common.http.AkkaHttpClient
import js7.common.system.startup.StartUp.printlnWithClock
import js7.common.system.startup.{JavaMain, StatefulServiceMain}
import monix.eval.Task
import monix.execution.Scheduler

object ClusterWatchMain
{
  def main(args: Array[String]): Unit = {
    printlnWithClock(s"JS7 ClusterWatch ${BuildInfo.longVersion}")
    val arguments = CommandLineArguments(args.toVector)
    //lockAndRunMain(args) { arguments =>
      val conf = ClusterWatchConf.fromCommandLine(arguments)
      JavaMain.runMain(conf.name, arguments, conf.config) {
        run(conf)(_ => Task.never)
      }
    //}
  }

  def run(conf: ClusterWatchConf)(use: ClusterWatchService => Task[Unit]): Unit =
    StatefulServiceMain.run(
      conf.name, conf.config,
      serviceResource = resource(conf, _),
      use = use)

  private def resource(conf: ClusterWatchConf, scheduler: Scheduler)
  : Resource[Task, ClusterWatchService] = {
    import conf.{clusterNodeAdmissions, config, httpsConfig}
    for {
      akka <- actorSystemResource("ClusterWatch", config, scheduler)
      service <- ClusterWatchService.resource(
        apiResources = clusterNodeAdmissions
          .traverse(admission => AkkaHttpClient
            .resource(admission.uri, uriPrefixPath = "", httpsConfig, name = "ClusterNode")(akka)
            .flatMap(HttpClusterNodeApi.resource(admission, _, uriPrefix = "controller"))),
        config)
    } yield service
  }
}
