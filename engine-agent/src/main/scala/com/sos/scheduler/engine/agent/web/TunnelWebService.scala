package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.web.common.AgentWebService
import com.sos.scheduler.engine.http.server.heartbeat.HeartbeatService
import com.sos.scheduler.engine.tunnel.data.{TunnelHandlerOverview, TunnelId, TunnelOverview, TunnelToken}
import com.sos.scheduler.engine.tunnel.server.TunnelAccess
import com.sos.scheduler.engine.tunnel.web.TunnelWebServices._
import java.time.Duration
import scala.collection.immutable
import scala.concurrent.Future
import spray.http.CacheDirectives.`max-age`
import spray.http.HttpHeaders.`Cache-Control`
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait TunnelWebService extends AgentWebService {

  protected def tunnelAccess(tunnelToken: TunnelToken): TunnelAccess
  protected def onTunnelHeartbeat(tunnelToken: TunnelToken, timeout: Duration): Unit
  protected def tunnelHandlerOverview: Future[TunnelHandlerOverview]
  protected def tunnelOverviews: Future[immutable.Iterable[TunnelOverview]]

  private implicit val executionContext = actorRefFactory.dispatcher

  addApiRoute {
    pathPrefix("tunnel") {
      path(Segment) { idString ⇒
        post {
          tunnelRequestRoute(TunnelId(idString))(tunnelAccess, onTunnelHeartbeat)
        }
      } ~
      respondWithHeader(`Cache-Control`(`max-age`(0))) {
        pathEnd {
          get {
            tunnelHandlerOverviewRouteComplete(tunnelHandlerOverview)
          }
        } ~
        pathSingleSlash  {
          get {
            tunnelOverviewsRouteComplete(tunnelOverviews)
          }
        }
      }
    }
  }
}
