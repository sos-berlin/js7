package js7.cluster.watch

import cats.effect.Resource
import js7.base.BuildInfo
import js7.base.service.{RestartAfterFailureService, Service}
import js7.cluster.watch.api.HttpClusterNodeApi
import js7.common.akkautils.Akkas.actorSystemResource
import js7.common.commandline.CommandLineArguments
import js7.common.http.AkkaHttpClient
import js7.base.system.startup.StartUp.printlnWithClock
import js7.common.system.startup.{JavaMain, ServiceMain}
import monix.eval.Task
import monix.execution.Scheduler

object ClusterWatchMain
{
  def main(args: Array[String]): Unit = {
    printlnWithClock(s"JS7 ClusterWatch ${BuildInfo.longVersion}")
    // Lazy, otherwise Log4j may be used and implicitly start uninitialized
    lazy val arguments = CommandLineArguments(args.toVector)
    lazy val conf = ClusterWatchConf.fromCommandLine(arguments)
    JavaMain.runMain("JS7 ClusterWatch", arguments, conf.config) {
      run(conf)(_ => Task.never)
    }
  }

  def run(conf: ClusterWatchConf)(use: Service => Task[Unit]): Unit =
    ServiceMain.run(
      name = conf.clusterWatchId.toString,
      conf.config,
      serviceResource = resource(conf, _),
      use = use)

  private def resource(conf: ClusterWatchConf, scheduler: Scheduler)
  : Resource[Task, RestartAfterFailureService[ClusterWatchService]] = {
    import conf.{clusterNodeAdmissions, config, httpsConfig}
    for {
      akka <- actorSystemResource(name = "ClusterWatch", config, scheduler)
      service <- ClusterWatchService.restartableResource(
        conf.clusterWatchId,
        apiResources = clusterNodeAdmissions
          .traverse(admission => AkkaHttpClient
            .resource(admission.uri, uriPrefixPath = "", httpsConfig, name = "ClusterNode")(akka)
            .flatMap(HttpClusterNodeApi.resource(admission, _, uriPrefix = "controller"))),
        config)
    } yield service
  }
}
