package js7.proxy

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO, Resource, ResourceIO}
import js7.base.catsutils.CatsEffectExtensions.unsafeRuntime
import js7.base.log.reader.LogDirectoryMXBean
import js7.base.service.{MainService, Service}
import js7.base.utils.ProgramTermination
import js7.common.pekkohttp.web.session.{SessionRegister, SimpleSession}
import js7.common.pekkoutils.Pekkos
import js7.common.system.startup.ServiceApp
import js7.controller.client.PekkoHttpControllerApi.admissionsToApiResource
import js7.data.state.EngineStateMXBean
import js7.proxy.Proxy.*
import js7.proxy.web.ProxyWebServer
import org.apache.pekko.actor.ActorSystem
import org.jetbrains.annotations.TestOnly

final class Proxy private(val controllerProxy: ControllerProxy)
extends MainService, Service.StoppableByCancel:

  protected type Termination = ProgramTermination

  export controllerProxy.metrics

  val untilTerminated: IO[ProgramTermination] =
    untilStopped.as(ProgramTermination())

  protected def start =
    startService:
      untilStopRequested


object Proxy extends ServiceApp:
  // No Logger here!

  def run(args: List[String]): IO[ExitCode] =
    runService(args, ProxyMainConf.fromCommandLine):
      Proxy.completeResource

  /** @param singleton Only on Proxy may run in a JVM (or ClassLoader) as a singleton with a ProxyId. */
  @TestOnly
  def runAsTest(args: List[String], singleton: Boolean = false)
    (use: Proxy => IO[ProgramTermination]): IO[ExitCode] =
    runService(
      args,
      a =>
        // See makeSingleton, only one Proxy in a JVM is allowed to have a ProxyId
        val conf = ProxyMainConf.fromCommandLine(a)
        if singleton then conf else conf.copy(proxyId = None),
      suppressLogShutdown = true)(
      Proxy.completeResource,
      use = (_, service: Proxy) => use(service))

  private def completeResource(conf: ProxyMainConf): ResourceIO[Proxy] =
    for
      given IORuntime <- Resource.eval(IO.unsafeRuntime)
      given ActorSystem <- Pekkos.actorSystemResource("Proxy")
      sessionRegister <- SessionRegister.service(SimpleSession(_), conf.config)
      apisResource = admissionsToApiResource(conf.admissions, conf.httpsConfig)
      controllerApi <- ControllerApi.resource(apisResource, conf.proxyId, conf.proxyConf)
      clusterWatch <- conf.clusterWatchId.fold(Resource.unit[IO]): clusterWatchId =>
        controllerApi.clusterWatchResource(clusterWatchId, config = conf.config)
      _ <- EngineStateMXBean.register
      controllerProxy <- controllerApi.controllerProxy()
      _ <- ProxyWebServer.service(controllerApi, sessionRegister, conf)
      _ <- LogDirectoryMXBean.register[IO](conf.logDirectory)
      service <- Service(Proxy(controllerProxy))
    yield
      controllerApi.setActive(true) // THIS MUST BE THE ONLY PROXY!
      service
