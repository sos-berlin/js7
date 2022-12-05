package js7.cluster.watch

import cats.effect.Resource
import js7.cluster.watch.api.HttpClusterNodeApi
import js7.common.akkautils.Akkas.actorSystemResource
import js7.common.http.AkkaHttpClient
import js7.common.system.startup.{JavaMain, StatefulServiceMain}
import monix.eval.Task
import monix.execution.Scheduler

object ClusterWatchMain
{
  def main(args: Array[String]): Unit = {
    val conf = ClusterWatchConf.fromCommandLine(args)
    JavaMain.runMain(conf.name, conf.config) {
      run(ClusterWatchConf.fromCommandLine(args))(_.untilStopped)
    }
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
