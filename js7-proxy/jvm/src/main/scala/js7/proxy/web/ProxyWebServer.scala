package js7.proxy.web

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, IO, ResourceIO}
import js7.base.auth.SimpleUser
import js7.common.pekkohttp.WebLogDirectives
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.session.{SessionRegister, SimpleSession}
import js7.proxy.{ControllerApi, ProxyMainConf}
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route

object ProxyWebServer:

  def service(
    controllerApi: ControllerApi,
    sessionRegister: SessionRegister[SimpleSession],
    conf: ProxyMainConf)
    (using ActorSystem, IORuntime)
  : ResourceIO[PekkoWebServer] =
    PekkoWebServer.service(conf.webServerBindings, conf.config): routeBinding =>
      PekkoWebServer.BoundRoute.simple(conf):
        new ProxyRoute(controllerApi, sessionRegister):
          protected def commonConf = conf
          protected val js7ServerId = conf.maybeJs7ServerId
          protected val gateKeeper = GateKeeper(routeBinding.webServerBinding, conf, SimpleUser.apply)
          override def toString = "Proxy web services"
        .webServerRoute

  private trait ProxyRoute(
    protected val controllerApi: ControllerApi,
    protected val sessionRegister: SessionRegister[SimpleSession])
    (using
      protected val ioRuntime: IORuntime,
      protected val actorSystem: ActorSystem)
  extends
    ProxyRouteProvider,
    WebLogDirectives,
    //LogRoute,
    ProxyMetricsRoute:

    override protected type OurSession = SimpleSession

    protected val whenShuttingDown = Deferred.unsafe/*???*/

    def webServerRoute: Route =
      mainRoute:
        pathPrefix(Segment):
          case "metrics" => proxyMetricsRoute
          //case "log" => logRoute
          case _ => reject
