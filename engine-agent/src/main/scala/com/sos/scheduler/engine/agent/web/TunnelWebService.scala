package com.sos.scheduler.engine.agent.web

import akka.util.ByteString
import com.sos.scheduler.engine.agent.web.common.AgentWebService
import com.sos.scheduler.engine.http.server.heartbeat.HeartbeatService
import com.sos.scheduler.engine.tunnel.data.{TunnelHandlerOverview, TunnelId, TunnelOverview, TunnelToken}
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

  protected def tunnelRequest(tunnelToken: TunnelToken, requestMessage: ByteString, timeout: Option[Duration]): Future[ByteString]
  protected def onTunnelHeartbeat(tunnelToken: TunnelToken): Unit
  protected def tunnelHandlerOverview: Future[TunnelHandlerOverview]
  protected def tunnelOverviews: Future[immutable.Iterable[TunnelOverview]]
  protected def heartbeatService: HeartbeatService

  private implicit val executionContext = actorRefFactory.dispatcher

  addApiRoute {
    pathPrefix("tunnel") {
      path(Segment) { idString ⇒
        post {
          tunnelRequestRoute(TunnelId(idString))(tunnelRequest, onTunnelHeartbeat, heartbeatService)
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
