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
import js7.proxy.web.ProxyWebServer
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.util.ByteString
import org.jetbrains.annotations.TestOnly

final class Proxy private(
  val controllerProxy: ControllerProxy,
  @TestOnly val metricsForTest: fs2.Stream[IO, ByteString])
extends MainService, Service.StoppableByCancel:

  protected type Termination = ProgramTermination

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

  @TestOnly
  def runAsTest(args: List[String])
    (use: Proxy => IO[ProgramTermination]): IO[ExitCode] =
    runService(
      args,
      a =>
        // See JProxyContext.makeSingleton, only one Proxy in a JVM is allowed to have a ProxyId.
        // Therefore, we omit groupAndProxyId while testing concurrently.
        ProxyMainConf.fromCommandLine(a).copy(groupAndProxyId = None),
      suppressLogShutdown = true)(
      Proxy.completeResource,
      use = (_, service: Proxy) => use(service))

  def completeResource(conf: ProxyMainConf): ResourceIO[Proxy] =
    for
      given IORuntime <- Resource.eval(IO.unsafeRuntime)
      given ActorSystem <- Pekkos.actorSystemResource("Proxy")
      sessionRegister <- SessionRegister.service(SimpleSession(_), conf.config)
      apisResource = admissionsToApiResource(conf.admissions, conf.httpsConfig)
      controllerApiRegister = new ControllerApiRegister(conf.groupAndProxyId)
      controllerApi <- ControllerApi.resource(
        apisResource,
        Some(controllerApiRegister),
        conf.proxyConf)
      clusterWatch <- conf.clusterWatchId.fold(Resource.unit[IO]): clusterWatchId =>
        controllerApi.clusterWatchResource(
          clusterWatchId,
          requireFailoverConfirmation = false,
          config = conf.config)
      _ <- EngineStateMXBean.register
      controllerProxy <- controllerApi.controllerProxy()
      _ <-
        ProxyWebServer.service(
          controllerApiRegister.metrics(deep = true),
          sessionRegister, conf,
          groupAndServerId = conf.groupAndProxyId.map(_.toGroupAndServerId))
      _ <- LogDirectoryMXBean.register[IO](conf.logDirectory)
      service <- Service(Proxy(controllerProxy, controllerApiRegister.metrics(deep = true)/*test only*/))
    yield
      controllerApi.allowEngineMetrics(true)
      service
