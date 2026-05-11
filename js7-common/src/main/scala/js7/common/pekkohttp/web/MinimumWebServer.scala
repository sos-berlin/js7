package js7.common.pekkohttp.web

import cats.effect.unsafe.IORuntime
import cats.effect.{Resource, ResourceIO}
import io.circe.Encoder
import js7.base.catsutils.Environment.environment
import js7.common.configuration.CommonConfiguration
import js7.common.metrics.MetricsRoute
import js7.common.pekkohttp.WebLogDirectives
import js7.common.pekkohttp.web.PekkoWebServer.RouteBinding
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.session.{RouteProvider, SessionRegister, SimpleSession}
import js7.common.web.serviceprovider.ServiceProviderRoute
import js7.data.node.GroupAndServerId
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route

object MinimumWebServer:

  def service(conf: CommonConfiguration, groupAndServerId: Option[GroupAndServerId] = None)
    (using actorSystem: ActorSystem)
  : ResourceIO[PekkoWebServer] =
    for
      given IORuntime <- Resource.eval(environment[IORuntime])
      sessionRegister <- SessionRegister.service(SimpleSession(_), conf.config)
      webServer <-
        PekkoWebServer.simple(conf): routeBinding =>
          MinimumRoute(sessionRegister, routeBinding, conf, groupAndServerId)
            .webServerRoute
    yield
      webServer


  private class MinimumRoute(
    protected val sessionRegister: SessionRegister[SimpleSession],
    routeBinding: RouteBinding,
    protected val commonConf: CommonConfiguration,
    protected val groupAndServerId: Option[GroupAndServerId] = None)
    (using
      protected val ioRuntime: IORuntime,
      protected val actorSystem: ActorSystem)
  extends
    ServiceProviderRoute,
    RouteProvider,
    WebLogDirectives,
    MetricsRoute:

    protected type OurSession = SimpleSession
    protected val sessionEncoder = summon[Encoder.AsObject[SimpleSession]]

    protected val gateKeeper = GateKeeper(
      routeBinding.webServerBinding,
      GateKeeper.Configuration.fromConfig(config))

    protected def whenShuttingDown = routeBinding.whenStopRequested

    val webServerRoute: Route =
      mainRoute:
        pathPrefix(Segment):
          case "metrics" => metricsRoute
          case _ => reject
        ~ serviceProviderRoute

    override def toString = s"${commonConf.name} web services"
