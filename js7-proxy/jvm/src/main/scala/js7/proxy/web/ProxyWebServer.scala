package js7.proxy.web

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, IO, ResourceIO}
import js7.base.auth.SimpleUser
import js7.common.pekkohttp.WebLogDirectives
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.session.{SessionRegister, SimpleSession}
import js7.data.node.GroupAndServerId
import js7.proxy.ProxyMainConf
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.util.ByteString

object ProxyWebServer:

  def service(
    metrics: fs2.Stream[IO, ByteString],
    sessionRegister: SessionRegister[SimpleSession],
    conf: ProxyMainConf,
    groupAndServerId: Option[GroupAndServerId] = None)
    (using ActorSystem, IORuntime)
  : ResourceIO[PekkoWebServer] =
    val groupAndServerId_ = groupAndServerId
    PekkoWebServer.service(conf.webServerBindings, conf.config): routeBinding =>
      PekkoWebServer.BoundRoute.simple(conf):
        new ProxyRoute(metrics, sessionRegister):
          protected def commonConf = conf
          protected def groupAndServerId = groupAndServerId_
          protected val gateKeeper = GateKeeper(routeBinding.webServerBinding, conf, SimpleUser.apply)
          override def toString = "Proxy web services"
        .webServerRoute

  private trait ProxyRoute(
    protected val metrics: fs2.Stream[IO, ByteString],
    protected val sessionRegister: SessionRegister[SimpleSession])
    (using
      protected val ioRuntime: IORuntime,
      protected val actorSystem: ActorSystem)
  extends
    ProxyRouteProvider,
    WebLogDirectives,
    //LogRoute,
    ProxyMetricsRoute:

    protected val whenShuttingDown = Deferred.unsafe/*???*/

    def webServerRoute: Route =
      mainRoute:
        pathPrefix(Segment):
          case "metrics" => proxyMetricsRoute
          //case "log" => logRoute
          case _ => reject
