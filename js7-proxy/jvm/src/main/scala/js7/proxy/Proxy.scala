package js7.proxy

import cats.effect.{ExitCode, IO, Resource, ResourceIO}
import js7.base.service.{MainService, Service}
import js7.base.utils.ProgramTermination
import js7.cluster.watch.ClusterWatchService
import js7.common.pekkoutils.Pekkos
import js7.common.system.startup.ServiceApp
import js7.controller.client.PekkoHttpControllerApi.admissionsToApiResource
import js7.proxy.Proxy.*
import org.apache.pekko.actor.ActorSystem
import org.jetbrains.annotations.TestOnly

final class Proxy private(controllerApi: ControllerApi)
extends MainService, Service.StoppableByCancel:

  protected type Termination = ProgramTermination

  val untilTerminated: IO[ProgramTermination] =
    untilStopped.as(ProgramTermination())

  protected def start =
    startService:
      controllerApi.proxyResource().surround:
        untilStopRequested


object Proxy extends ServiceApp:
  // No Logger here!

  def run(args: List[String]): IO[ExitCode] =
    runService(args, ProxyMainConf.fromCommandLine):
      Proxy.completeResource

  @TestOnly
  def runAsTest(args: List[String])(use: Proxy => IO[ProgramTermination]): IO[ExitCode] =
    runService(args, ProxyMainConf.fromCommandLine, suppressLogShutdown = true)(
      Proxy.completeResource,
      use = (_, service: Proxy) => use(service))

  private def completeResource(conf: ProxyMainConf): ResourceIO[Proxy] =
    for
      given ActorSystem <- Pekkos.actorSystemResource("Proxy")
      apisResource = admissionsToApiResource(conf.admissions, conf.httpsConfig)
      controllerApi <- ControllerApi.resource(apisResource, conf.proxyConf)
      clusterWatch <- conf.clusterWatchId.fold(Resource.unit[IO]): clusterWatchId =>
        ClusterWatchService.resource(clusterWatchId, apisResource, conf.config)
      service <- Service.resource(Proxy(controllerApi))
    yield
      service
