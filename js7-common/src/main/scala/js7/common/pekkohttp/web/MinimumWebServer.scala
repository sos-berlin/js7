package js7.common.pekkohttp.web

import cats.effect.unsafe.IORuntime
import cats.effect.{Resource, ResourceIO}
import js7.base.auth.SimpleUser
import js7.base.catsutils.Environment.environment
import js7.common.configuration.CommonConfiguration
import js7.common.pekkohttp.WebLogDirectives
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.PekkoWebServer.RouteBinding
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.session.{RouteProvider, SessionRegister, SimpleSession}
import js7.common.web.serviceprovider.ServiceProviderRoute
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.server.Route

object MinimumWebServer:

  def service(conf: CommonConfiguration)(using actorSystem: ActorSystem)
  : ResourceIO[PekkoWebServer] =
    for
      given IORuntime <- Resource.eval(environment[IORuntime])
      sessionRegister <- SessionRegister.resource(SimpleSession(_), conf.config)
      webServer <- PekkoWebServer.simple(conf):
        routeBinding => MinimumRoute(sessionRegister, routeBinding, conf).webServerRoute
    yield
      webServer


  private class MinimumRoute(
    protected val sessionRegister: SessionRegister[SimpleSession],
    routeBinding: RouteBinding,
    conf: CommonConfiguration)
    (using
      protected val ioRuntime: IORuntime,
      protected val actorSystem: ActorSystem)
  extends
    ServiceProviderRoute,
    RouteProvider,
    WebLogDirectives:

    protected type OurSession = SimpleSession

    protected val gateKeeper = GateKeeper(
      routeBinding.webServerBinding,
      GateKeeper.Configuration.fromConfig(config))
    protected def config = conf.config
    protected val commonConf = conf

    protected def whenShuttingDown = routeBinding.whenStopRequested

    val webServerRoute: Route =
      mainRoute:
        serviceProviderRoute

    override def toString = s"${conf.name} web services"
