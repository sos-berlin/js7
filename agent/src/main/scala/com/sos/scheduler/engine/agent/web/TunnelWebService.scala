package com.sos.scheduler.engine.agent.web

import akka.actor.ActorRefFactory
import com.sos.scheduler.engine.agent.web.common.AgentWebService
import com.sos.scheduler.engine.common.sprayutils.SprayJsonOrYamlSupport._
import com.sos.scheduler.engine.common.sprayutils.SprayUtils.pathSegments
import com.sos.scheduler.engine.tunnel.data.TunnelView._
import com.sos.scheduler.engine.tunnel.data._
import com.sos.scheduler.engine.tunnel.server.TunnelAccess
import com.sos.scheduler.engine.tunnel.web.TunnelWebServices._
import java.time.Duration
import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}
import spray.http.CacheDirectives.`max-age`
import spray.http.HttpHeaders.`Cache-Control`
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait TunnelWebService extends AgentWebService {

  protected implicit def actorRefFactory: ActorRefFactory
  protected implicit def executionContext: ExecutionContext
  protected def tunnelAccess(tunnelToken: TunnelToken): TunnelAccess
  protected def onTunnelHeartbeat(tunnelToken: TunnelToken, timeout: Duration): Unit
  protected def tunnelHandlerOverview: Future[TunnelHandlerOverview]
  protected def tunnelOverviews: Future[immutable.Iterable[TunnelOverview]]
  protected def tunnelView(tunnelId: TunnelId): Future[TunnelView]

  routeBuilder.addApiRoute { _ ⇒
    pathSegments("tunnel") {
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
        } ~
        path(Segment) { idString ⇒
          val tunnelId = TunnelId(idString)
          (pathEnd & get) {
            complete(tunnelView(tunnelId): Future[TunnelView])
          }
        }
      }
    }
  }
}
