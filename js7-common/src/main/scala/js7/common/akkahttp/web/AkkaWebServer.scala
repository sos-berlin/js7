package js7.common.akkahttp.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.Route
import cats.effect.Resource
import cats.syntax.all.*
import com.typesafe.config.{Config, ConfigFactory}
import js7.base.configutils.Configs.*
import js7.base.problem.Problems.WebServiceStillNotAvailableProblem
import js7.base.service.Service
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.common.akkahttp.StandardMarshallers.*
import js7.common.akkahttp.web.data.{WebServerBinding, WebServerPort}
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import monix.eval.Task
import org.jetbrains.annotations.TestOnly
import scala.concurrent.Future
import scala.concurrent.duration.Deadline

/**
 * @author Joacim Zschimmer
 */
final class AkkaWebServer private(
  protected val webServerPorts: Seq[WebServerPort])
extends WebServerBinding.HasLocalUris
with Service.StoppableByRequest
{
  protected def start =
    startService(untilStopRequested)

  override def toString =
    s"AkkaWebServer(${webServerPorts mkString " "})"
}

object AkkaWebServer
{
  private[web] val testConfig = config"""
    js7.web.server.auth.https-client-authentication = off
    js7.web.server.shutdown-timeout = 10s
    """

  @TestOnly
  def testResource()(route: Route)(implicit as: ActorSystem): Resource[Task, AkkaWebServer] =
    testUriAndResource()(route)._2

  @TestOnly
  def testUriAndResource()(route: Route)(implicit as: ActorSystem)
  : (Uri, Resource[Task, AkkaWebServer]) =
    testResource(ConfigFactory.empty)(route)

  @TestOnly
  def testResource(config: Config)(route: Route)(implicit as: ActorSystem)
  : (Uri, Resource[Task, AkkaWebServer]) =
    testResource(findFreeTcpPort(), config, route = route)

  @TestOnly
  def testResource(port: Int = findFreeTcpPort(), config: Config = ConfigFactory.empty, route: Route)(implicit as: ActorSystem)
  : (Uri, Resource[Task, AkkaWebServer]) =
    Uri(s"http://127.0.0.1:$port") -> httpResource(port = port, config.withFallback(testConfig), route)

  def httpResource(port: Int, config: Config, route: Route)
    (implicit actorSystem: ActorSystem)
  : Resource[Task, AkkaWebServer] =
    resource(
      Seq(WebServerBinding.http(port)),
      config,
      (_, _) => BoundRoute.simple(route))

  def resource(
    webServerBindings: Seq[WebServerBinding],
    config: Config,
    toBoundRoute: (WebServerBinding, Future[Deadline]) => BoundRoute)
    (implicit actorSystem: ActorSystem)
  : Resource[Task, AkkaWebServer] =
    Resource.suspend(Task {
      val shutdownTimeout = config.finiteDuration("js7.web.server.shutdown-timeout").orThrow
      val httpsClientAuthRequired = config.getBoolean(
        "js7.web.server.auth.https-client-authentication")

      webServerBindings
        .parTraverse(webServerBinding =>
          SinglePortAkkaWebServer
            .resource(
              webServerBinding,
              toBoundRoute(webServerBinding, _),
              shutdownTimeout = shutdownTimeout,
              httpsClientAuthRequired = httpsClientAuthRequired)
            .as(webServerBinding.toWebServerPort))
        .flatMap(webServerPorts =>
          Service.resource(Task(
            new AkkaWebServer(webServerPorts))))
    })

  private lazy val stillNotAvailableRoute: Route =
    complete(WebServiceStillNotAvailableProblem)

  trait BoundRoute {
    def serviceName: String

    def stillNotAvailableRoute: Route =
      AkkaWebServer.stillNotAvailableRoute

    def webServerRoute: Task[Route]

    def startupSecurityHint(scheme: WebServerBinding.Scheme): String
  }
  object BoundRoute {
    def simple(route: Route): BoundRoute =
      new Simple(route)

    final class Simple(route: Route) extends BoundRoute {
      def serviceName = ""

      def webServerRoute = Task.pure(route)

      def startupSecurityHint(scheme: WebServerBinding.Scheme) = ""
    }
  }
}
